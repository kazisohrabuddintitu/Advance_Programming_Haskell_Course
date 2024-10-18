module SPC
  ( -- * SPC startup
    SPC(..),
    startSPC,

    -- * Job functions
    Job (..),
    JobId,
    JobStatus (..),
    JobDoneReason (..),
    jobAdd,
    jobStatus,
    jobWait,
    jobCancel,

    -- * Worker functions
    WorkerName,
    workerAdd,
    workerStop,
    Worker(..),

    -- * Message types
    WorkerMsg(..), 
    SPCMsg(..),

    -- * Functions
    getWorkers,
    getServer,

  )
where
import GenServer
import qualified Control.Concurrent as CC
import Control.Concurrent
  ( ThreadId,
    forkIO,
    killThread,
    threadDelay,
  )
import Control.Exception (SomeException, onException, catch)
import Data.List (partition)
import Control.Monad (ap, forM_, forever, liftM, void)
import System.Clock.Seconds (Clock (Monotonic), Seconds, getTime)

-- | Retrieve Unix time using a monotonic clock. You cannot use this
-- to measure the actual world time, but you can use it to measure
-- elapsed time.
getSeconds :: IO Seconds
getSeconds = getTime Monotonic

-- | Remove mapping from association list.
removeAssoc :: (Eq k) => k -> [(k, v)] -> [(k, v)]
removeAssoc needle ((k, v) : kvs) =
  if k == needle
    then kvs
    else (k, v) : removeAssoc needle kvs
removeAssoc _ [] = []

-- Then the definition of the glorious SPC.

-- | A job that is to be enqueued in the glorious SPC.
data Job = Job
  { -- | The IO action that comprises the actual action of the job.
    jobAction :: IO (),
    -- | The maximum allowed runtime of the job, counting from when
    -- the job begins executing (not when it is enqueued).
    jobMaxSeconds :: Int
  }

-- | A unique identifier of a job that has been enqueued.
newtype JobId = JobId Int
  deriving (Eq, Ord, Show)

-- | How a job finished.
data JobDoneReason
  = -- | Normal termination.
    Done
  | -- | The job was killed because it ran for too long.
    DoneTimeout
  | -- | The job was explicitly cancelled, or the worker
    -- it was running on was stopped.
    DoneCancelled
  | -- | The job crashed due to an exception.
    DoneCrashed
  deriving (Eq, Ord, Show)

-- | The status of a job.
data JobStatus
  = -- | The job is done and this is why.
    JobDone JobDoneReason
  | -- | The job is still running.
    JobRunning
  | -- | The job is enqueued, but is waiting for an idle worker.
    JobPending
  | -- | A job with this ID is not known to this SPC instance.
    JobUnknown
  deriving (Eq, Ord, Show)

-- | A worker decides its own human-readable name. This is useful for
-- debugging.
type WorkerName = String

-- | Messages sent to workers. These are sent both by SPC and by
-- processes spawned by the workes.
data WorkerMsg
  = -- | Handle a job message.
    MsgJob JobId
  | -- | Stop the worker.
    MsgStop
  deriving (Show)


-- Messages sent to SPC.
data SPCMsg
  = -- | Add the job, and reply with the job ID.
    MsgJobAdd Job (ReplyChan JobId)
  | -- | Cancel the given job.
    MsgJobCancel JobId
  | -- | Immediately reply the status of the job.
    MsgJobStatus JobId (ReplyChan JobStatus)
  | -- | Reply when the job is done.
    MsgJobWait JobId (ReplyChan JobDoneReason)
  | -- | Job has finished.
    MsgJobDone JobId
  | -- | Job crashed.
    MsgJobCrashed JobId
  | -- | Some time has passed.
    MsgTick
  | -- | Check if a worker with this name exists.
    MsgWorkerExists WorkerName (ReplyChan Bool)
  | -- | Add a new worker to the SPC.
    MsgAddWorker WorkerName Worker (ReplyChan ())
  | MsgWorkerStop WorkerName
  | MsgGetWorkers (ReplyChan [(WorkerName, Worker)])

-- | A handle to the SPC instance.
data SPC = SPC (Server SPCMsg)

-- | A handle to a worker.
data Worker = Worker (Server WorkerMsg)

-- | The central state. Must be protected from the bourgeoisie.
data SPCState = SPCState
  { spcJobsPending :: [(JobId, Job)],
    spcJobsRunning :: [(JobId, Job)],
    spcJobRunning :: Maybe (JobId, Seconds, ThreadId),
    spcJobsDone :: [(JobId, JobDoneReason)],
    spcJobCounter :: JobId,
    spcWaiting :: [(JobId, ReplyChan (Maybe JobDoneReason))],
    spcChan :: Chan SPCMsg,
    -- TODO: you will need to add more fields.
    spcWorkers :: [(WorkerName, Worker)] 
  }

-- | The monad in which the main SPC thread runs. This is a state
-- monad with support for IO.
newtype SPCM a = SPCM (SPCState -> IO (a, SPCState))

instance Functor SPCM where
  fmap = liftM

instance Applicative SPCM where
  pure x = SPCM $ \state -> pure (x, state)
  (<*>) = ap

instance Monad SPCM where
  SPCM m >>= f = SPCM $ \state -> do
    (x, state') <- m state
    let SPCM f' = f x
    f' state'

-- | Retrieve the state.
get :: SPCM SPCState
get = SPCM $ \state -> pure (state, state)

-- | Overwrite the state.
put :: SPCState -> SPCM ()
put state = SPCM $ \_ -> pure ((), state)

getWorkers :: SPC -> IO [(WorkerName, Worker)]
getWorkers (SPC c) = requestReply c $ \replyChan -> MsgGetWorkers replyChan

getServer :: SPC -> Server SPCMsg
getServer (SPC server) = server

-- | Modify the state.
modify :: (SPCState -> SPCState) -> SPCM ()
modify f = do
  state <- get
  put $ f state

-- | Lift an 'IO' action into 'SPCM'.
io :: IO a -> SPCM a
io m = SPCM $ \state -> do
  x <- m
  pure (x, state)

-- | Run the SPCM monad.
runSPCM :: SPCState -> SPCM a -> IO a
runSPCM state (SPCM f) = fst <$> f state

schedule :: SPCM ()
schedule = do
  state <- get
  case (spcJobRunning state, spcJobsPending state) of
    (Nothing, (jobid, job) : jobs) -> do
      now <- io getSeconds
      tid <- io $ forkIO $ do
        let doJob = do
              jobAction job
              send (spcChan state) $ MsgJobDone jobid
        onException doJob (send (spcChan state) $ MsgJobCrashed jobid)
      let deadline = now + fromIntegral (jobMaxSeconds job)
      put $ state
        { spcJobRunning = Just (jobid, deadline, tid),
          spcJobsPending = jobs,
          spcJobsRunning = spcJobsRunning state,
          spcJobsDone = spcJobsDone state,
          spcJobCounter = spcJobCounter state,
          spcWaiting = spcWaiting state,
          spcChan = spcChan state,
          spcWorkers = spcWorkers state 
        }
    _ -> pure ()


jobDone :: JobId -> JobDoneReason -> SPCM ()
jobDone jobid reason = do
  state <- get
  case lookup jobid $ spcJobsDone state of
    Just _ ->
      -- We already know this job is done.
      pure ()
    Nothing -> do
      let (waiting_for_job, not_waiting_for_job) =
            partition ((== jobid) . fst) (spcWaiting state)
      forM_ waiting_for_job $ \(_, rsvp) ->
        io $ reply rsvp $ Just reason
      put $
        state
          { spcWaiting = not_waiting_for_job,
            spcJobsDone = (jobid, reason) : spcJobsDone state,
            spcJobsPending = removeAssoc jobid $ spcJobsPending state
          }

workerIsIdle :: WorkerName -> Worker -> SPCM ()
workerIsIdle = undefined

workerIsGone :: WorkerName -> SPCM ()
workerIsGone = undefined

checkTimeouts :: SPCM ()
checkTimeouts = pure () -- change in Task 4

workerExists :: WorkerName -> SPCM Bool
workerExists name = do
  state <- get
  return $ any ((== name) . fst) (spcWorkers state)

handleMsg :: Chan SPCMsg -> SPCM ()
handleMsg c = do
  checkTimeouts
  schedule
  msg <- io $ receive c
  case msg of
    MsgJobAdd job rsvp -> do
      state <- get
      let JobId jobid = spcJobCounter state
      put $
        state
          { spcJobsPending =
              (spcJobCounter state, job) : spcJobsPending state,
            spcJobCounter = JobId $ succ jobid
          }
      io $ reply rsvp $ JobId jobid
    
    MsgJobStatus jobid rsvp -> do
      state <- get
      io $ reply rsvp $ case ( lookup jobid $ spcJobsPending state,
                               lookup jobid $ spcJobsRunning state,
                               lookup jobid $ spcJobsDone state
                             ) of
        (Just _, _, _) -> JobPending
        (_, Just _, _) -> JobRunning
        (_, _, Just r) -> JobDone r
        _ -> JobUnknown
    
    MsgWorkerExists name replyChan -> do
      exists <- workerExists name
      io $ reply replyChan exists

    MsgAddWorker name worker replyChan -> do
      modify $ \s -> s { spcWorkers = (name, worker) : spcWorkers s }
      io $ reply replyChan ()
    
    MsgWorkerStop name -> do
      state <- get
      case lookup name (spcWorkers state) of
        Just worker -> do
          io $ workerStop worker
          modify $ \s -> s { spcWorkers = filter ((/= name) . fst) (spcWorkers s) } 
        Nothing -> io $ putStrLn $ "Worker with name " ++ name ++ " does not exist."
    
    MsgGetWorkers replyChan -> do
      state <- get
      io $ reply replyChan (spcWorkers state)
    
    MsgJobDone done_jobid -> do
      state <- get
      case spcJobRunning state of
        Just (jobid, _, _)
          | jobid == done_jobid ->
              jobDone jobid Done
        _ -> pure ()
    
    MsgJobCrashed crashed_jobid -> do
      state <- get
      case spcJobRunning state of
        Just (jobid, _, tid) | jobid == crashed_jobid -> do
          io $ killThread tid
          jobDone jobid DoneCrashed
        _ -> pure ()

startSPC :: IO SPC
startSPC = do
  let initial_state =
        SPCState
          { spcJobCounter = JobId 0,
            spcJobsPending = [],
            spcJobsRunning = [],
            spcJobsDone = [],
            spcJobRunning = Nothing,
            spcWaiting = [],
            spcWorkers = []
            
          }
  c <- spawn $ \c -> runSPCM initial_state $ forever $ handleMsg c
  void $ spawn $ timer c
  pure $ SPC c
  where
    timer c _ = forever $ do
      threadDelay 1000000 -- 1 second
      sendTo c MsgTick

-- | Add a job for scheduling.
jobAdd :: SPC -> Job -> IO JobId
jobAdd (SPC c) job =
  requestReply c $ MsgJobAdd job

-- | Asynchronously query the job status.
jobStatus :: SPC -> JobId -> IO JobStatus
jobStatus (SPC c) jobid =
  requestReply c $ MsgJobStatus jobid

-- | Synchronously block until job is done and return the reason.
jobWait :: SPC -> JobId -> IO JobDoneReason
jobWait (SPC c) jobid =
  requestReply c $ MsgJobWait jobid

-- | Asynchronously cancel a job.
jobCancel :: SPC -> JobId -> IO ()
jobCancel (SPC c) jobid =
  sendTo c $ MsgJobCancel jobid

-- | Add a new worker with this name. Fails with 'Left' if a worker
workerAdd :: SPC -> WorkerName -> IO (Either String Worker)
workerAdd (SPC c) name = do
  exists <- requestReply c $ \replyChan -> MsgWorkerExists name replyChan
  if exists
    then return $ Left "Worker with this name already exists."
    else do
      newWorkerChan <- CC.newChan
      tid <- CC.forkIO $ workerThread newWorkerChan
      let newWorker = Worker (Server tid newWorkerChan)
      requestReply c $ \replyChan -> MsgAddWorker name newWorker replyChan
      return $ Right newWorker


-- | The worker thread function where the worker processes jobs.
workerThread :: Chan WorkerMsg -> IO ()
workerThread input = forever $ do
  msg <- receive input
  handleWorkerMsg msg

-- | Handle messages for the worker.
handleWorkerMsg :: WorkerMsg -> IO ()
handleWorkerMsg (MsgJob jobId) = do
  putStrLn $ "Processing job: " ++ show jobId
  -- Simulate job processing.
  threadDelay 1000000
  -- Here you would send back a message indicating job completion.
  
handleWorkerMsg MsgStop = do
  putStrLn "Stopping worker."
  -- Clean up resources if necessary
  -- Exit the worker thread (the loop will terminate)


-- | Shut down a running worker. No effect if the worker is already
workerStop :: Worker -> IO ()
workerStop (Worker (GenServer.Server t input)) = do
  send input MsgStop
