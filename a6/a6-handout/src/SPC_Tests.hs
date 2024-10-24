module SPC_Tests (tests) where

import Control.Concurrent (threadDelay)
-- import Control.Monad
import Data.IORef
import SPC
import GenServer
import Test.Tasty (TestTree, localOption, mkTimeout, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Data.Maybe ()




tests :: TestTree
tests =
  localOption (mkTimeout 3000000) $
    testGroup
      "SPC (core)"
      [ 
        testCase "adding job" $ do
          spc <- startSPC
          _ <- jobAdd spc $ Job (pure ()) 1
          pure ()
        
        , testCase "adding job" $ do
          spc <- startSPC
          j <- jobAdd spc $ Job (pure()) 1
          r <- jobStatus spc j
          r @?= Just JobRunning

        , testCase "adding worker" $ do
          spc <- startSPC
          let workerName = "worker1"
          _ <- workerAdd spc workerName
          pure ()  
        
        , testCase "worker-exists" $ do
          spc <- startSPC
          let workerName = "worker2"
          _ <- workerAdd spc workerName
          exists <- requestReply (getServer spc) $ \replyChan -> MsgWorkerExists workerName replyChan
          exists @?= True  

        , testCase "canceling job" $ do
          spc <- startSPC
          j <- jobAdd spc $ Job (pure ()) 5
          jobCancel spc j
          threadDelay 1000000
          r <- jobStatus spc j
          r @?= Just (JobDone Done)
        
        , testCase "timeout" $ do
          spc <- startSPC
          ref <- newIORef False
          j <- jobAdd spc $ Job (threadDelay 2000000 >> writeIORef ref True) 1
          r1 <- jobStatus spc j
          r1 @?= Just JobRunning
          r2 <- jobWait spc j
          r2 @?= Just DoneTimeout 
        
        , testCase "crash" $ do
          spc <- startSPC
          j1 <- jobAdd spc $ Job (error "boom") 1
          r1 <- jobWait spc j1
          r1 @?= Just DoneCrashed
          -- Ensure new jobs can still work.
          ref <- newIORef False
          j2 <- jobAdd spc $ Job (writeIORef ref True) 1
          r2 <- jobWait spc j2
          r2 @?= Just Done
          v <- readIORef ref
          v @?= True
        
        -- , testCase "job crashes" $ do
        --   spc <- startSPC
        --   let workerName = "worker3"
        --   _ <- workerAdd spc workerName
        --   j <- jobAdd spc $ Job (error "Job crashed") 1
        --   threadDelay 100000
        --   r1 <- jobWait spc j
        --   r1 @?= Just DoneCrashed
        --   workerStatus <- requestReply (getServer spc) $ \replyChan -> MsgWorkerExists workerName replyChan
        --   workerStatus @?= True
        
        -- , testCase "Removing Workers" $ do
          spc <- startSPC
          let workerName = "worker3"
          
          -- Add worker
          _ <- workerAdd spc workerName

          -- Add a job
          j <- jobAdd spc $ Job (pure ()) 1  

          -- Stop the worker and cancel the job
          _ <- requestReply (getServer spc) $ \replyChan -> 
              MsgWorkerStop workerName j replyChan  -- Pass the worker here

          -- Wait for the worker to process the stop and cancellation
          threadDelay 10000

          -- Check the job status
          r <- jobStatus spc j
          r @?= Nothing  -- Ensure job is now unknown


      ]
