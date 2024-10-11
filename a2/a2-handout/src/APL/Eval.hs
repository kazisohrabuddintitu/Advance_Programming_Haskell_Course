module APL.Eval
  ( Val (..),
    eval,
    runEval,
    Error,
  )
where

import APL.AST (Exp (..), VName)
import Control.Monad (ap, liftM)

data Val
  = ValInt Integer
  | ValBool Bool
  | ValFun Env VName Exp
  deriving (Eq, Show)

type Env = [(VName, Val)]
type Store = [(Val, Val)]

envEmpty :: Env
envEmpty = []

envExtend :: VName -> Val -> Env -> Env
envExtend v val env = (v, val) : env

envLookup :: VName -> Env -> Maybe Val
envLookup v env = lookup v env

type Error = String

newtype EvalM a = EvalM (Env -> Store -> ([String], Either Error a, Store))

instance Functor EvalM where
  fmap = liftM

instance Applicative EvalM where
  pure x = EvalM $ \_env store -> ([], Right x, store)
  (<*>) = ap

instance Monad EvalM where
  EvalM x >>= f = EvalM $ \env store ->
    let (log1, res1, store1) = x env store
    in case res1 of
         Left err -> (log1, Left err, store1)
         Right x' ->
           let EvalM y = f x'
               (log2, res2, store2) = y env store1
           in (log1 ++ log2, res2, store2)


askEnv :: EvalM Env
askEnv = EvalM $ \env store -> ([], Right env, store)

localEnv :: (Env -> Env) -> EvalM a -> EvalM a
localEnv f (EvalM m) = EvalM $ \env store -> m (f env) store

failure :: String -> EvalM a
failure s = EvalM $ \_env store -> ([], Left s, store)

catch :: EvalM a -> EvalM a -> EvalM a
catch (EvalM m1) (EvalM m2) = EvalM $ \env store ->
  let (log1, res1, store1) = m1 env store
  in case res1 of
       Left _ -> let (log2, res2, store2) = m2 env store1
                 in (log1 ++ log2, res2, store2)
       Right val -> (log1, Right val, store1)

runEval :: EvalM a -> ([String], Either Error a)
runEval (EvalM m) = 
  let (log, result, _) = m envEmpty []
  in (log, result)

evalIntBinOp :: (Integer -> Integer -> EvalM Integer) -> Exp -> Exp -> EvalM Val
evalIntBinOp f e1 e2 = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValInt x, ValInt y) -> ValInt <$> f x y
    (_, _) -> failure "Non-integer operand"

evalIntBinOp' :: (Integer -> Integer -> Integer) -> Exp -> Exp -> EvalM Val
evalIntBinOp' f e1 e2 =
  evalIntBinOp f' e1 e2
  where
    f' x y = pure $ f x y

evalPrint :: String -> EvalM ()
evalPrint s = EvalM $ \_env store -> ([s], Right (), store)

evalKvPut :: Val -> Val -> EvalM ()
evalKvPut key val = EvalM $ \_env store ->
  ([], Right (), (key, val) : filter ((/= key) . fst) store)  -- Replacing if key exists

evalKvGet :: Val -> EvalM Val
evalKvGet key = EvalM $ \_env store ->
  case lookup key store of
    Just val -> ([], Right val, store)
    Nothing  -> ([], Left $ "Invalid key: " ++ show key, store)

eval :: Exp -> EvalM Val
eval (CstInt x) = pure $ ValInt x
eval (CstBool b) = pure $ ValBool b
eval (Var v) = do
  env <- askEnv
  case envLookup v env of
    Just x -> pure x
    Nothing -> failure $ "Unknown variable: " ++ v
eval (Add e1 e2) = evalIntBinOp' (+) e1 e2
eval (Sub e1 e2) = evalIntBinOp' (-) e1 e2
eval (Mul e1 e2) = evalIntBinOp' (*) e1 e2
eval (Div e1 e2) = evalIntBinOp checkedDiv e1 e2
  where
    checkedDiv _ 0 = failure "Division by zero"
    checkedDiv x y = pure $ x `div` y
eval (Pow e1 e2) = evalIntBinOp checkedPow e1 e2
  where
    checkedPow x y =
      if y < 0
        then failure "Negative exponent"
        else pure $ x ^ y
eval (Eql e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValInt x, ValInt y) -> pure $ ValBool $ x == y
    (ValBool x, ValBool y) -> pure $ ValBool $ x == y
    (_, _) -> failure "Invalid operands to equality"
eval (If cond e1 e2) = do
  cond' <- eval cond
  case cond' of
    ValBool True -> eval e1
    ValBool False -> eval e2
    _ -> failure "Non-boolean conditional."
eval (Let var e1 e2) = do
  v1 <- eval e1
  localEnv (envExtend var v1) $ eval e2
eval (Lambda var body) = do
  env <- askEnv
  pure $ ValFun env var body
eval (Apply e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValFun f_env var body, arg) ->
      localEnv (const $ envExtend var arg f_env) $ eval body
    (_, _) ->
      failure "Cannot apply non-function"
eval (TryCatch e1 e2) =
  eval e1 `catch` eval e2
eval (Print s e) = do
  v <- eval e
  let output = case v of
        ValInt n  -> show n
        ValBool b -> show b
        ValFun _ _ _ -> "#<fun>"
  evalPrint (s ++ ": " ++ output)
  pure v
eval (KvPut k_exp v_exp) = do
  k <- eval k_exp
  v <- eval v_exp
  evalKvPut k v
  pure v
eval (KvGet k_exp) = do
  k <- eval k_exp
  evalKvGet k
