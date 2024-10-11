module APL.InterpIO (runEvalIO) where

import APL.Monad
import APL.Util
import System.Directory (removeFile)
import System.IO (hFlush, readFile', stdout, withFile, IOMode(ReadMode, WriteMode), hGetContents, hPutStr)


-- Converts a string into a value. Only 'ValInt's and 'ValBool' are supported.
readVal :: String -> Maybe Val
readVal = unserialize

-- 'prompt s' prints 's' to the console and then reads a line from stdin.
prompt :: String -> IO String
prompt s = do
  putStr s
  hFlush stdout
  getLine

-- 'writeDB dbFile s' writes the 'State' 's' to the file 'db'.
writeDB :: FilePath -> State -> IO ()
writeDB db s =
  writeFile db $ serialize s 

-- 'readDB db' reads the database stored in 'db'.
readDB :: FilePath -> IO (Either Error State)
readDB db = do
  ms <- readFile' db
  case unserialize ms of
    Just s -> pure $ Right s 
    Nothing -> pure $ Left "Invalid DB."

-- Removes all key-value pairs from the database file.
clearDB :: IO ()
clearDB = writeFile dbFile ""

-- The name of the database file.
dbFile :: FilePath
dbFile = "db.txt"

-- Creates a fresh temporary database, passes it to a function returning an
-- IO-computation, executes the computation, deletes the temporary database, and
-- finally returns the result of the computation. The temporary database file is
-- guaranteed fresh and won't have a name conflict with any other files.
withTempDB :: (FilePath -> IO a) -> IO a
withTempDB m = do
  tempDB <- newTempDB -- Create a new temp database file.
  res <- m tempDB -- Run the computation with the new file.
  removeFile tempDB -- Delete the temp database file.
  pure res -- Return the result of the computation.

replaceState :: State -> (Val,Val) -> State
replaceState [] (k, v) = [(k, v)]
replaceState ((k', v') : xs) (k, v)
  | k == k'  = (k, v) : xs
  | otherwise = (k', v') : replaceState xs (k, v)

-- 'copyDB src dst' copies the contents of 'src' file to 'dst' file.
copyDB :: FilePath -> FilePath -> IO ()
copyDB src dst = do
  withFile src ReadMode $ \hSrc -> do
    content <- hGetContents hSrc
    withFile dst WriteMode $ \hDst -> do
      hPutStr hDst content


runEvalIO :: EvalM a -> IO (Either Error a)
runEvalIO evalm = do
  clearDB
  runEvalIO' envEmpty dbFile evalm
  where
    runEvalIO' :: Env -> FilePath -> EvalM a -> IO (Either Error a)
    runEvalIO' _ _ (Pure x) = pure $ pure x
    runEvalIO' r db (Free (ReadOp k)) = runEvalIO' r db $ k r
    runEvalIO' r db (Free (StateGetOp k)) = do
      dbState <- readDB db
      case dbState of
          Left err -> return $ Left err 
          Right state -> runEvalIO' r db (k state) 
    runEvalIO' r db (Free (StatePutOp s m)) = do
      writeDB db s  
      runEvalIO' r db m 
    runEvalIO' r db (Free (PrintOp p m)) = do
      putStrLn p
      runEvalIO' r db m
    runEvalIO' _ _ (Free (ErrorOp e)) = pure $ Left e
    runEvalIO' r db (Free (TryCatchOp m1 m2)) = do
      res1 <- runEvalIO' r db m1
      case res1 of
        Right val -> pure $ Right val
        Left _ -> runEvalIO' r db m2
    runEvalIO' r db (Free (KvGetOp v1 k)) = do
      (Right state) <- readDB db
      case lookup v1 state of
          Just x -> runEvalIO' r db (k x)
          Nothing -> do
            prm <- prompt ("Invalid key: " ++ show v1) 
            case readVal prm of
              Just x' -> runEvalIO' r db (k x')
              Nothing -> pure $ Left $ "Invalid value input: " ++ prm
    runEvalIO' r db (Free (KvPutOp v1 v2 m)) = do
      (Right state) <- readDB db
      writeDB db (replaceState state (v1,v2) ) 
      runEvalIO' r db m
    runEvalIO' r db (Free (TransactionOp action m)) = withTempDB $ \tempDB -> do
      -- Copy the current state to the tempDB
      copyDB db tempDB
      -- Execute the transaction in the tempDB context
      result <- runEvalIO' r tempDB action
      case result of
        Right _ -> do
          -- If successful, copy the tempDB back to the main db
          copyDB tempDB db
          runEvalIO' r db m
        Left _ -> do
          -- If failed, discard changes and continue
          runEvalIO' r db m