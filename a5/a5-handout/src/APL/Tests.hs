{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module APL.Tests
  ( properties
  )
where

import APL.AST (Exp (..), subExp, VName, printExp)
import APL.Error (isVariableError, isDomainError, isTypeError)
import APL.Check (checkExp)
import APL.Parser (parseAPL)
import Test.QuickCheck
  ( Property
  , Gen
  , Arbitrary (arbitrary, shrink)
  , property
  , cover
  , oneof
  , checkCoverage
  , sized
  , elements
  , frequency
  )

instance Arbitrary Exp where
  arbitrary = sized (\n -> genExp n [])
  
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

-- -- Enhanced variable generation logic
-- genVar :: Gen VName
-- genVar = listOf1 (elements ['a'..'z']) >>= \name -> return name

genExp :: Int -> [VName] -> Gen Exp
genExp 0 _ = frequency
  [ (10, CstInt <$> arbitrary)  -- 10%
  , (10, CstBool <$> arbitrary) -- 10%
  ]

genExp size vars = frequency
  [ (10, CstInt <$> arbitrary)  -- 10%
  , (10, CstBool <$> arbitrary) -- 10%

  -- Operations that do not cause errors
  , (10, Add <$> genExp halfSize vars <*> genExp halfSize vars)   -- 10%
  , (10, Sub <$> genExp halfSize vars <*> genExp halfSize vars)   -- 10%
  , (10, Mul <$> genExp halfSize vars <*> genExp halfSize vars)   -- 10%

  -- Domain error cases
  , (20, Div <$> genExp halfSize vars <*> pure (CstInt 0))          -- 20% (div by zero)
  , (20, Pow <$> genExp halfSize vars <*> pure (CstInt (-1)))      -- 20% (negative exponent)

  -- Additional domain error scenarios
  , (10, Div <$> genExp halfSize vars <*> oneof [pure (CstInt 0), genExp halfSize vars]) -- 10%
  , (5, Pow <$> genExp halfSize vars <*> genExp halfSize vars)     -- 5%

  -- Equal and control structures
  , (10, Eql <$> genExp halfSize vars <*> genExp halfSize vars)     -- 10%
  , (20, If <$> genExp (size `div` 3) vars <*> genExp (size `div` 3) vars <*> genExp (size `div` 3) vars) -- 20%

  -- Increased frequency for Apply and TryCatch to enhance coverage
  , (20, Apply <$> genExp halfSize vars <*> genExp halfSize vars) -- 20%
  , (20, TryCatch <$> genExp halfSize vars <*> genExp halfSize vars) -- 20%

  -- Reduced variable cases to lower variable errors
  , (5, Var <$> elements (if null vars then ["x", "y", "z", "ab", "bc"] else vars))   -- 5%
  , (5, Let <$> elements (if null vars then ["x", "y", "z", "ab", "bc"] else vars) <*> genExp halfSize vars <*> genExp halfSize vars) -- 5%
  , (5, Lambda <$> elements (if null vars then ["x", "y", "z", "ab", "bc"] else vars) <*> genExp (size - 1) vars) -- 5%

  -- More varied variable names to increase non-trivial variable coverage
  , (10, Var <$> elements (if null vars then ["ab", "bc", "cd", "xyz", "def"] else vars))  -- 10%
  ]
  where
    halfSize = size `div` 2


-- genExp size vars = frequency
--   [ (10, CstInt <$> arbitrary)
--   , (10, CstBool <$> arbitrary)
--   , (15, Add <$> genExp halfSize vars <*> genExp halfSize vars)   -- 15%
--   , (15, Sub <$> genExp halfSize vars <*> genExp halfSize vars)   -- 15%
--   , (15, Mul <$> genExp halfSize vars <*> genExp halfSize vars)   -- 15%
--   , (25, Div <$> genExp halfSize vars <*> pure (CstInt 0))          -- 25% (div by zero)
--   , (25, Pow <$> genExp halfSize vars <*> pure (CstInt (-1)))
--   , (20, Div <$> genExp halfSize vars <*> oneof [pure (CstInt 0), genExp halfSize vars]) -- 20%
--   , (20, Pow <$> genExp halfSize vars <*> genExp halfSize vars)
--   , (10, Eql <$> genExp halfSize vars <*> genExp halfSize vars)     -- 10%
--   , (15, If <$> genExp (size `div` 3) vars <*> genExp (size `div` 3) vars <*> genExp (size `div` 3) vars) -- 15%
--   , (5, Var <$> elements (if null vars then ["x", "y", "z", "ab", "bc"] else vars))   -- 5%
--   , (5, Let <$> elements (if null vars then ["x", "y", "z", "ab", "bc"] else vars) <*> genExp halfSize vars <*> genExp halfSize vars) -- 5%
--   , (5, Lambda <$> elements (if null vars then ["x", "y", "z", "ab", "bc"] else vars) <*> genExp (size - 1) vars) -- 5%
--   , (20, Apply <$> genExp halfSize vars <*> genExp halfSize vars) -- 20%
--   , (20, TryCatch <$> genExp halfSize vars <*> genExp halfSize vars) -- 20%
--   , (10, Var <$> elements (if null vars then ["ab", "bc", "cd", "xyz", "def"] else vars))  -- 10%
--   ]
--   where
--     halfSize = size `div` 2

expCoverage :: Exp -> Property
expCoverage e = 
    let errors = checkExp [] e 
    in checkCoverage
        . cover 20 (any isDomainError errors) "domain error"
        . cover 20 (not $ any isDomainError errors) "no domain error"
        . cover 20 (any isTypeError errors) "type error"
        . cover 20 (not $ any isTypeError errors) "no type error"
        . cover 5 (any isVariableError errors) "variable error"
        . cover 70 (not $ any isVariableError errors) "no variable error"
        . cover 50 (any isVarInRange (subExp e)) "non-trivial variable"
        $ ()
  where
    isVarInRange (Var v) = length v >= 2 && length v <= 4
    isVarInRange _        = False

parsePrinted :: Exp -> Bool
parsePrinted e =
    case parseAPL "" (printExp e) of
        Left _ -> False
        Right e' -> e == e'


onlyCheckedErrors :: Exp -> Bool
onlyCheckedErrors e =
    let errors = checkExp [] e
    in all isCheckedError errors
  where
    isCheckedError err = isDomainError err || isTypeError err || isVariableError err

properties :: [(String, Property)]
properties =
  [ ("expCoverage", property expCoverage)
  , ("parsePrinted", property parsePrinted)
  , ("onlyCheckedErrors", property onlyCheckedErrors)
  ]