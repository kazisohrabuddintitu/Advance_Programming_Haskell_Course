module APL.Check (checkExp, Error) where

import APL.AST (Exp (..), VName)

type Error = String
type CheckEnv = [VName]

newtype CheckM a = CheckM { runCheck :: CheckEnv -> Maybe Error } -- TODO - give this a proper definition.

instance Functor CheckM where
  fmap _ (CheckM m) = CheckM m

instance Applicative CheckM where
  pure _ = CheckM $ \_ -> Nothing
  CheckM f <*> CheckM x = CheckM $ \env ->
    case f env of
      Nothing -> x env
      err     -> err

instance Monad CheckM where
  CheckM m >>= f = CheckM $ \env ->
    case m env of
      Nothing -> runCheck (f undefined) env
      err     -> err


checkExp :: Exp -> Maybe Error
checkExp exp = runCheck (check exp) []

check :: Exp -> CheckM ()
check (CstInt _) = pure ()
check (CstBool _) = pure ()
check (Var v) = CheckM $ \env ->
  if v `elem` env
  then Nothing
  else Just $ "Variable not in scope: " ++ v
check (Lambda v body) = local (v :) (check body)
check (Let v e1 e2) = do
  check e1
  local (v :) (check e2)
check (Add e1 e2) = checkBinOp e1 e2
check (Sub e1 e2) = checkBinOp e1 e2
check (Mul e1 e2) = checkBinOp e1 e2
check (Div e1 e2) = checkBinOp e1 e2
check (Pow e1 e2) = checkBinOp e1 e2
check (Eql e1 e2) = checkBinOp e1 e2
check (If cond e1 e2) = do
  check cond
  check e1
  check e2
check (Apply e1 e2) = do
  check e1
  check e2
check (Print _ e) = check e
check (KvPut k_exp v_exp) = do
  check k_exp
  check v_exp
check (KvGet k_exp) = check k_exp

checkBinOp :: Exp -> Exp -> CheckM ()
checkBinOp e1 e2 = do
  check e1
  check e2

local :: (CheckEnv -> CheckEnv) -> CheckM a -> CheckM a
local f (CheckM m) = CheckM $ \env -> m (f env)

