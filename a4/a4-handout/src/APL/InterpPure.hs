module APL.InterpPure (runEval) where

import APL.Monad

replaceState :: State -> (Val,Val) -> State
replaceState [] (k, v) = [(k, v)]
replaceState ((k', v') : xs) (k, v)
  | k == k'  = (k, v) : xs
  | otherwise = (k', v') : replaceState xs (k, v)

runEval :: EvalM a -> ([String], Either Error a)
runEval = runEval' envEmpty stateInitial
  where
    runEval' :: Env -> State -> EvalM a -> ([String], Either Error a)
    
    runEval' _ _ (Pure x) = ([], Right x) 
    runEval' r s (Free (ReadOp k)) = runEval' r s (k r)
    runEval' r s (Free (StateGetOp k)) = runEval' r s (k s)
    runEval' r _ (Free (StatePutOp s' m)) = runEval' r s' m
    runEval' r s (Free (PrintOp p m)) =
      let (ps, res) = runEval' r s m
      in (p : ps, res)  
    runEval' _ _ (Free (ErrorOp e)) = ([], Left e) 
    runEval' r s (Free (TryCatchOp m1 m2)) =
      let (ps1, res1) = runEval' r s m1
      in case res1 of
           Right val -> (ps1, Right val) 
           Left _ -> runEval' r s m2   
    runEval' r s (Free (KvGetOp v1 k)) =
      case lookup v1 s of
        Just v2 -> runEval' r s (k v2)
        Nothing -> ([], Left ("Invalid key: " ++ show v1))
    runEval' r s (Free (KvPutOp v1 v2 k)) =
      runEval' r (replaceState s (v1, v2)) k
    runEval' r s (Free (TransactionOp m k)) =
      let (ps1, res1) = runEval' r s m 
      in case res1 of
          Right () -> 
            let (ps2, res2) = runEval' r s k
            in (ps1 ++ ps2, res2) 
          Left e  -> 
            let (ps2, res2) = runEval' r s k
            in (ps1 ++ ps2, res2)