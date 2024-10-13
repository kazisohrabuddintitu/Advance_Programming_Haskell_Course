{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Evaluate" #-}
module APL.Tests
  ( properties
  )
where

import APL.AST (Exp (..), subExp, VName)
import APL.Error (isVariableError, isDomainError, isTypeError)
import APL.Check (checkExp)
import APL.Parser (parseAPL)
import APL.Eval
import Test.QuickCheck
  ( Property
  , Gen
  , Arbitrary (arbitrary, shrink)
  , property
  , cover
  , checkCoverage
  , sized
  , elements
  , frequency
  , suchThat
  , vectorOf
  , chooseInt
  )


instance Arbitrary Exp where
  arbitrary = sized (genExp [] ) 

  shrink (Add e1 e2) = e1 : e2 : [Add e1' e2 | e1' <- shrink e1] ++ [Add e1 e2' | e2' <- shrink e2]
  shrink (Sub e1 e2) = e1 : e2 : [Sub e1' e2 | e1' <- shrink e1] ++ [Sub e1 e2' | e2' <- shrink e2]
  shrink (Mul e1 e2) = e1 : e2 : [Mul e1' e2 | e1' <- shrink e1] ++ [Mul e1 e2' | e2' <- shrink e2]
  shrink (Div e1 e2) = e1 : e2 : [Div e1' e2 | e1' <- shrink e1] ++ [Div e1 e2' | e2' <- shrink e2]
  shrink (Pow e1 e2) = e1 : e2 : [Pow e1' e2 | e1' <- shrink e1] ++ [Pow e1 e2' | e2' <- shrink e2]
  shrink (Eql e1 e2) = e1 : e2 : [Eql e1' e2 | e1' <- shrink e1] ++ [Eql e1 e2' | e2' <- shrink e2]
  shrink (If cond e1 e2) = e1 : e2 : [If cond' e1 e2 | cond' <- shrink cond] ++ [If cond e1' e2 | e1' <- shrink e1] ++ [If cond e1 e2' | e2' <- shrink e2]
  shrink (Let x e1 e2) = e1 : [Let x e1' e2 | e1' <- shrink e1] ++ [Let x e1 e2' | e2' <- shrink e2]
  shrink (Lambda x e) = [Lambda x e' | e' <- shrink e]
  shrink (Apply e1 e2) = e1 : e2 : [Apply e1' e2 | e1' <- shrink e1] ++ [Apply e1 e2' | e2' <- shrink e2]
  shrink (TryCatch e1 e2) = e1 : e2 : [TryCatch e1' e2 | e1' <- shrink e1] ++ [TryCatch e1 e2' | e2' <- shrink e2]
  shrink _ = []


genVar :: Int ->  Gen VName
genVar i = do
    alpha <- elements ['a' .. 'z']
    x <- chooseInt (0, i)
    y <- chooseInt (0,4-1-x)
    alphas <- vectorOf x (elements ['a' .. 'z'])
    nums <- vectorOf y (elements  ['0' .. '9'])
    pure ([alpha] ++ alphas ++ nums)


genVar' :: [VName] ->  Gen VName
genVar' s = do
  case s of
    [] -> genVar 3
    _ ->  elements s


genPos :: Gen Integer
genPos = abs `fmap` (arbitrary :: Gen Integer) `suchThat` (> 0)

genLam :: [VName]->Int -> Gen Exp
genLam s size = do
  let vngen = genVar 2
  vname <- vngen
  Lambda vname <$> genExp (vname :s) (size - 1)


genExp :: [VName]->Int -> Gen Exp
genExp _ 0 = frequency [(6,CstInt <$> genPos), (2,CstBool <$> arbitrary)]
genExp s size =
  frequency $
    [ (80,CstInt <$> genPos)
    , (40,CstBool <$> arbitrary)
    , (30, Add <$> genExp s halfSize <*> genExp s halfSize)
    , (30,Sub <$> genExp s halfSize <*> genExp s halfSize)
    , (30,Mul <$> genExp s halfSize <*> genExp s halfSize)
    , (30,Div <$> genExp s halfSize <*> genExp s halfSize)
    , (30,Pow <$> genExp s halfSize <*> genExp s halfSize)
    , (30,Eql <$> genExp s halfSize <*> genExp s halfSize)
    , (30,If <$> genExp s thirdSize <*> genExp s thirdSize <*> genExp s thirdSize)
    , (5,Var <$> genVar 100)
    , (400, do
      let vngen = genVar 2
      vname <- vngen
      (Let vname <$> genExp s halfSize) <*> genExp (vname:s) halfSize)
    , (30,genLam s size)
    , (30, Apply <$> genLam s halfSize <*> genExp s halfSize)
    , (30,TryCatch <$> genExp s halfSize <*> genExp s halfSize)
    ] ++ case s of
      [] -> []
      _ -> [(60, Var <$> genVar' s)]
    where
    (halfSize, thirdSize) = (size `div` 2,size `div` 3)




expCoverage :: Exp -> Property
expCoverage e = checkCoverage
  . cover 20 (any isDomainError (checkExp e)) "domain error"
  . cover 20 (not $ any isDomainError (checkExp e)) "no domain error"
  . cover 20 (any isTypeError (checkExp e)) "type error"
  . cover 20 (not $ any isTypeError (checkExp e)) "no type error"
  . cover 5 (any isVariableError (checkExp e)) "variable error"
  . cover 70 (not $ any isVariableError (checkExp e)) "no variable error"
  . cover 50 (or [2 <= n && n <= 4 | Var v <- subExp e, let n = length v]) "non-trivial variable"
  $ ()


parsePrinted :: Exp -> Bool
parsePrinted e =
  case parseAPL "input" (show e) of
    Left _ -> False
    Right e' -> e == e'

onlyCheckedErrors :: Exp -> Bool
onlyCheckedErrors e =
  let checkedErrors = checkExp e
  in case runEval $ eval e of
    Left err -> err `elem` checkedErrors
    Right _ -> True


properties :: [(String, Property)]
properties =
  [ ("expCoverage", property expCoverage)
  , ("onlyCheckedErrors", property onlyCheckedErrors)
  , ("parsePrinted", property parsePrinted)
  ]

