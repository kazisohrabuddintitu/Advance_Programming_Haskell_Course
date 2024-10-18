{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
module SPC_Tests (tests) where

import Control.Concurrent (threadDelay)
import Control.Monad (forM, forM_, replicateM)
import Data.IORef
import SPC
import GenServer
import Test.Tasty (TestTree, localOption, mkTimeout, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  localOption (mkTimeout 3000000) $
    testGroup
      "SPC (core)"
      [ testCase "worker-add" $ do
          spc <- startSPC
          let workerName = "worker1"
          workerRes <- workerAdd spc workerName
          case workerRes of
            Right worker -> do
              exists <- requestReply (getServer spc) $ \replyChan -> MsgWorkerExists workerName replyChan
              exists @?= True -- Ensure worker exists after adding
            Left errMsg -> assertFailure $ "Worker addition failed: " ++ errMsg

      , testCase "worker-exists" $ do
          spc <- startSPC
          let workerName = "worker2"
          _ <- workerAdd spc workerName
          exists <- requestReply (getServer spc) $ \replyChan -> MsgWorkerExists workerName replyChan
          exists @?= True -- Check that the worker exists

      , testCase "worker-stop" $ do
          spc <- startSPC
          let workerName = "worker3"
          _ <- workerAdd spc workerName
          let job = Job (threadDelay 1000000) 1 -- Add a dummy job
          jobId <- jobAdd spc job -- Enqueue the job

          workers <- getWorkers spc  -- Use the function to get workers
          case workers of
            ((name, workerRef):_) -> do
              -- Send a stop message to the worker using the server reference
              let (Worker workerServer) = workerRef
              sendTo workerServer MsgStop

              exists <- requestReply (getServer spc) $ \replyChan -> MsgWorkerExists name replyChan
              exists @?= False -- Check that the worker no longer exists after stopping
            [] -> assertFailure "No workers available to stop."

      , testCase "stop-non-existent-worker" $ do
          spc <- startSPC
          let nonExistentWorkerName = "worker4"
          _ <- requestReply (getServer spc) $ \replyChan -> MsgWorkerStop nonExistentWorkerName
          -- Ensure that trying to stop a non-existent worker does not fail
          return ()
      ]
