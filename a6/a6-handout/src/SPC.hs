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
import Control.Exception (onException)
import Data.List (partition)
import Control.Monad (ap, forM_, forever, liftM, void)
import System.Clock.Seconds (Clock (Monotonic), Seconds, getTime)
import Data.Maybe ()


-- | Retrieve Unix time using a monotonic clock. You cannot use this
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
    MsgJobStatus JobId (ReplyChan (Maybe JobStatus))
  | -- | Reply when the job is done.
    MsgJobWait JobId (ReplyChan (Maybe JobDoneReason))
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
  | MsgWorkerStop WorkerName JobId (ReplyChan ())
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
      t <- io $ forkIO $ do
        let doJob = do
              jobAction job
              send (spcChan state) $ MsgJobDone jobid
        onException doJob (send (spcChan state) $ MsgJobCrashed jobid)
      let deadline = now + fromIntegral (jobMaxSeconds job)
      put $ state
        { spcJobRunning = Just (jobid, deadline, t),
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
    -- If the job is already marked as done, do nothing.
    Just _ -> pure ()
    Nothing -> do
      -- Find clients waiting for the job to finish.
      let (waiting_for_job, not_waiting_for_job) =
            partition ((== jobid) . fst) (spcWaiting state)
      
      -- Reply to all clients waiting for the job with the reason.
      forM_ waiting_for_job $ \(_, rsvp) ->
        io $ reply rsvp $ Just reason
      
      -- Update the state.
      put $
        state
          { spcWaiting = not_waiting_for_job,
            spcJobsDone = (jobid, reason) : spcJobsDone state,
            spcJobsPending = removeAssoc jobid $ spcJobsPending state,
            -- Only remove the running job if it matches the completed job.
            spcJobRunning = case spcJobRunning state of
              Just (runningJobId, _, _) | runningJobId == jobid -> Nothing
              other -> other
          }


workerIsIdle :: WorkerName -> Worker -> SPCM ()
workerIsIdle name worker = do
  state <- get
  put $ state { spcJobRunning = Nothing }
  schedule 

-- workerIsGone :: WorkerName -> SPCM ()
-- workerIsGone = undefined

checkTimeouts :: SPCM ()
checkTimeouts = do
  state <- get
  now <- io getSeconds
  case spcJobRunning state of
    Just (jobid, deadline, tid)
      | now >= deadline -> do
          io $ killThread tid
          jobDone jobid DoneTimeout
          case spcWorkers state of
            (workerName, worker):_ -> workerIsIdle workerName worker
            _ -> pure ()
    _ -> pure ()

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
      let newPendingJobs = (JobId jobid, job) : spcJobsPending state
      put $ state
        { spcJobsPending = newPendingJobs,
          spcJobCounter = JobId $ succ jobid
        }
      
      io $ reply rsvp $ JobId jobid

    MsgJobStatus jobid rsvp -> do
        state <- get
        io $ reply rsvp $ case (lookup jobid $ spcJobsPending state,
                                lookup jobid $ spcJobsDone state,
                                spcJobRunning state) of
            (Just _, _, _) -> Just JobPending  -- Job is pending
            (_, Just doneReason, _) -> Just (JobDone doneReason)
            (_, _, Just (runningJobId, _, _)) | runningJobId == jobid -> Just JobRunning  -- Job is currently running
            _ -> Nothing  -- Job is unknown or has been cancelled

      
    MsgWorkerExists name replyChan -> do
      exists <- workerExists name
      io $ reply replyChan exists

    MsgAddWorker name worker replyChan -> do
      modify $ \s -> s { spcWorkers = (name, worker) : spcWorkers s }
      io $ reply replyChan ()

    MsgWorkerStop name jobId replyChan -> do
      state <- get
      let spc = SPC (getServer spc)
      case lookup name (spcWorkers state) of
        Just worker -> do
          io $ jobCancel spc jobId
          io $ workerStop worker

          modify $ \s -> s { spcWorkers = filter ((/= name) . fst) (spcWorkers s) }

          io $ reply replyChan ()
        Nothing -> do
          io $ reply replyChan ()

    MsgGetWorkers replyChan -> do
      state <- get
      io $ reply replyChan (spcWorkers state)

    MsgJobDone done_jobid -> do
      state <- get
      case spcJobRunning state of
          Just (jobid, _, _) | jobid == done_jobid -> do
              -- Mark the job as done with the normal reason
              jobDone jobid Done
          _ -> pure ()


    MsgJobCrashed crashed_jobid -> do
      state <- get
      case spcJobRunning state of
          Just (jobid, _, _) | jobid == crashed_jobid -> do
              jobDone jobid DoneCrashed
              -- Set the worker to idle since the job has crashed
              case spcWorkers state of
                  (workerName, worker):_ -> workerIsIdle workerName worker
                  _ -> pure ()
          _ -> pure ()

    
    MsgJobCancel jobid -> do
      state <- get
      case lookup jobid $ spcJobsPending state of
        Nothing -> pure ()
        Just _ ->
          put $
            state
              { spcJobsPending = removeAssoc jobid $ spcJobsPending state,
                spcJobsDone = (jobid, DoneCancelled) : spcJobsDone state
              }

    MsgJobWait jobid rsvp -> do
      state <- get
      case lookup jobid (spcJobsDone state) of
          Just reason -> do
              io $ reply rsvp (Just reason)  -- Job is done, reply with the reason
          Nothing -> do
              -- If the job is not done, you may want to store the replyChan and handle it later.
              -- You might want to add it to a waiting list.
              let waiting = (jobid, rsvp) : spcWaiting state
              put state { spcWaiting = waiting }


    MsgTick ->
      pure ()

startSPC :: IO SPC
startSPC = do
  let initial_state c =
        SPCState
          { spcJobCounter = JobId 0,
            spcJobsPending = [],
            spcJobsRunning = [],
            spcJobsDone = [],
            spcJobRunning = Nothing,
            spcWaiting = [],
            spcWorkers = [],
            spcChan = c
            
          }
  server <- spawn $ \c -> runSPCM (initial_state c) $ forever $ handleMsg c
  void $ spawn $ timer server
  pure $ SPC server
  where
    timer server _ = forever $ do
      threadDelay 1000000 -- 1 second
      sendTo server MsgTick


-- | Add a job for scheduling.
jobAdd :: SPC -> Job -> IO JobId
jobAdd (SPC c) job =
  requestReply c $ MsgJobAdd job

-- | Asynchronously query the job status.

-- jobStatus :: SPC -> JobId -> IO (Maybe JobStatus)
-- jobStatus (SPC c) jobid = do
--   result <- requestReply c $ MsgJobStatus jobid 
--   return (Just result)                          

jobStatus :: SPC -> JobId -> IO (Maybe JobStatus)
jobStatus (SPC c) jobid =
  requestReply c $ \replyChan -> MsgJobStatus jobid replyChan


-- | Synchronously block until job is done and return the reason.
jobWait :: SPC -> JobId -> IO (Maybe JobDoneReason)
jobWait (SPC c) jobid = do
  requestReply c $ \replyChan -> MsgJobWait jobid replyChan
     

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
  threadDelay 1000000
  
handleWorkerMsg MsgStop = do
  putStrLn "Stopping worker."


-- | Shut down a running worker. No effect if the worker is already stopped.
workerStop :: Worker -> IO ()
workerStop (Worker (Server t input)) = do
  send input MsgStop        
  threadDelay 10000        
  killThread t             








-- jobDone :: JobId -> JobDoneReason -> SPCM ()
-- jobDone jobid reason = do
--   state <- get
--   case lookup jobid $ spcJobsDone state of
--     Just _ ->
--       pure ()
--     Nothing -> do
--       let (waiting_for_job, not_waiting_for_job) =
--             partition ((== jobid) . fst) (spcWaiting state)
--       forM_ waiting_for_job $ \(_, rsvp) ->
--         io $ reply rsvp $ Just reason
--       put $
--         state
--           { spcWaiting = not_waiting_for_job,
--             spcJobsDone = (jobid, reason) : spcJobsDone state,
--             spcJobsPending = removeAssoc jobid $ spcJobsPending state,
--             spcJobRunning = Nothing
--           }




-- MsgWorkerStop name -> do
--       state <- get
--       case lookup name (spcWorkers state) of
--         Just worker -> do
--           io $ workerStop worker
--           modify $ \s -> s { spcWorkers = filter ((/= name) . fst) (spcWorkers s) }
--         Nothing -> io $ putStrLn $ "Worker with name " ++ name ++ " does not exist."


-- -- | Shut down a running worker. No effect if the worker is already
-- workerStop :: Worker -> IO ()
-- workerStop (Worker (GenServer.Server t input)) = do
--   send input MsgStop        
--   threadDelay 10000         
--   killThread t             



-- checkTimeouts :: SPCM ()
-- checkTimeouts = do
--   state <- get
--   now <- io getSeconds
--   case spcJobRunning state of
--     Just (jobid, deadline, tid)
--       | now >= deadline -> do
--           io $ killThread tid
--           jobDone jobid DoneTimeout
--     _ -> pure ()


    -- MsgJobCancel cancel_jobid -> do
    --   state <- get
    --   case spcJobRunning state of
    --     Just (jobid, _, tid) | jobid == cancel_jobid -> do
    --       io $ killThread tid
    --       jobDone jobid DoneCancelled
    --     _ -> pure ()
    -- MsgJobCancel jobid -> do
    --   state <- get
    --   case lookup jobid $ spcJobsPending state of
    --     Nothing -> pure ()
    --     Just _ -> do
    --       let (waiting_for_jobid, rest) =
    --             partition ((== jobid) . fst) $ spcWaiting state
    --       forM_ waiting_for_jobid $ \(_, rsvp) ->
    --         io $ reply rsvp $ Just DoneCancelled
    --       put $
    --         state
    --           { spcJobsPending = removeAssoc jobid $ spcJobsPending state,
    --             spcJobsDone = (jobid, DoneCancelled) : spcJobsDone state,
    --             spcWaiting = rest
    --           }


    -- jobStatus :: SPC -> JobId -> IO (Maybe JobStatus)
-- jobStatus (SPC c) jobid = do
--   result <- requestReply c $ MsgJobStatus jobid
--   case result of
--     JobUnknown  -> return Nothing 
--     status      -> return (Just status)

-- jobStatus :: SPC -> JobId -> IO (Maybe JobStatus)
-- jobStatus (SPC c) jobid = do
--   result <- requestReply c $ MsgJobStatus jobid -- result is Maybe JobStatus
--   case result of
--     Just status -> return (Just status)         -- If result is Just, return it.
--     Nothing   -> return Nothing               -- If result is Nothing, return Nothing.


    -- MsgJobStatus jobid rsvp -> do
    --   state <- get
    --   io $ reply rsvp $ case lookup jobid $ spcJobsPending state of
    --     Just _ -> JobPending
    --     _ -> JobUnknown