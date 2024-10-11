module APL.Check_Tests (tests) where

import APL.AST (Exp (..))
import APL.Check (checkExp)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

-- Assert that the provided expression should pass the type checker.
testPos :: Exp -> TestTree
testPos e =
  testCase (show e) $
    checkExp e @?= Nothing

-- Assert that the provided expression should fail the type checker.
testNeg :: Exp -> TestTree
testNeg e =
  testCase (show e) $
    case checkExp e of
      Nothing -> assertFailure "expected error"
      Just _ -> pure ()

tests :: TestTree
tests =
  testGroup
    "Checking"
    [ testPos (Let "x" (CstInt 42) (Var "x")),
      --
      testPos (Lambda "y" (Add (Var "y") (CstInt 1))),
      --
      testPos (If (CstBool True) (CstInt 1) (CstInt 0)),
      --
      testNeg (Var "y"),
      --
      testNeg (Let "x" (CstInt 42) (Var "y")),
      --
      testNeg (Let "x" (CstInt 42) (Add (Var "x") (Var "y"))),
      --
      testNeg (Lambda "x" (Add (Var "x") (Var "y"))),
      --
      testNeg (If (CstBool True) (Var "x") (CstInt 0))
    ]
