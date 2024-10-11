module APL.AST_Tests (tests) where

import APL.AST (Exp (..), printExp)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Prettyprinting"
    [ 
      --
      testCase "Integer" $
        printExp (CstInt 8) @?= "8",
      --
      testCase "Boolean" $
        printExp (CstBool False) @?= "False",
      --
      testCase "Variable" $
        printExp (Var "z") @?= "z",
      --
      testCase "Add" $
        printExp (Add (CstInt 3) (CstInt 3)) @?= "(3 + 3)",
      --
      testCase "Sub" $
        printExp (Sub (CstInt 8) (CstInt 4)) @?= "(8 - 4)",
      --
      testCase "Mul" $
        printExp (Mul (CstInt 10) (CstInt 34)) @?= "(10 * 34)",
      --
      testCase "Div" $
        printExp (Div (CstInt 12) (CstInt 3)) @?= "(12 / 3)",
      --
      testCase "Pow" $
        printExp (Pow (CstInt 4) (CstInt 3)) @?= "(4 ** 3)",
      --
      testCase "Eql" $
        printExp (Eql (CstInt 10) (CstInt 10)) @?= "(10 == 10)",
      --
      testCase "If" $
        printExp (If (CstBool True) (CstInt 1) (CstInt 0)) @?= "(if True then 1 else 0)",
      --
      testCase "Let" $
        printExp (Let "z" (CstInt 6) (Add (Var "z") (CstInt 8))) @?= "(let z = 6 in (z + 8))",
      --
      testCase "Lambda" $
        printExp (Lambda "x" (Add (Var "x") (CstInt 1))) @?= "(\\ x -> (x + 1))",
      --
      testCase "Apply" $
        printExp (Apply (Lambda "x" (Add (Var "x") (CstInt 1))) (CstInt 5)) @?="(((\\ x -> (x + 1))) 5)",
      --
      testCase "TryCatch" $
        printExp (TryCatch (CstInt 1) (CstInt 2)) @?= "(try 1 catch 2)"
    ]
