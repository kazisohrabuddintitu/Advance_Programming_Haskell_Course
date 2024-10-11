module APL.Eval_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (Error, Val (..), eval, runEval)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Data.Either (Either(Right))

eval' :: Exp -> Either Error Val
eval' exp = snd (runEval (eval exp))

evalTests :: TestTree
evalTests =
  testGroup
    "EValuation"
    [ testCase "Add" $
        eval' (Add (CstInt 2) (CstInt 5))
          @?= Right (ValInt 7),
      --
      testCase "Add (wrong type)" $
        eval' (Add (CstInt 2) (CstBool True))
          @?=  Left "Non-integer operand",
      --
      testCase "Sub" $
        eval' (Sub (CstInt 2) (CstInt 5))
          @?= Right (ValInt (-3)),
      --
      testCase "Div" $
        eval' (Div (CstInt 7) (CstInt 3))
          @?= Right (ValInt 2),
      --
      testCase "Div0" $
        eval' (Div (CstInt 7) (CstInt 0))
          @?= Left "Division by zero",
      --
      testCase "Pow" $
        eval' (Pow (CstInt 2) (CstInt 3))
          @?= Right (ValInt 8),
      --
      testCase "Pow0" $
        eval' (Pow (CstInt 2) (CstInt 0))
          @?= Right (ValInt 1),
      --
      testCase "Pow negative" $
        eval' (Pow (CstInt 2) (CstInt (-1)))
          @?= Left "Negative exponent",
      --
      testCase "Eql (false)" $
        eval' (Eql (CstInt 2) (CstInt 3))
          @?= Right (ValBool False),
      --
      testCase "Eql (true)" $
        eval' (Eql (CstInt 2) (CstInt 2))
          @?= Right (ValBool True),
      --
      testCase "If" $
        eval' (If (CstBool True) (CstInt 2) (Div (CstInt 7) (CstInt 0)))
          @?= Right (ValInt 2),
      --
      testCase "Let" $
        eval' (Let "x" (Add (CstInt 2) (CstInt 3)) (Var "x"))
          @?= Right (ValInt 5),
      --
      testCase "Let (shadowing)" $
        eval'
          ( Let
              "x"
              (Add (CstInt 2) (CstInt 3))
              (Let "x" (CstBool True) (Var "x"))
          )
          @?= Right (ValBool True),
      --
      testCase "Lambda/Apply" $
        eval'
          (Apply (Lambda "x" (Mul (Var "x") (Var "x"))) (CstInt 4))
          @?= Right (ValInt 16),
      --
      testCase "TryCatch" $
        eval'
          (TryCatch (Div (CstInt 7) (CstInt 0)) (CstBool True))
          @?= Right (ValBool True),
      
      --Add more
      --
      testCase "Print" $
        runEval (eval (Print "foo" (CstInt 2)))
          @?= (["foo: 2"], Right (ValInt 2)),
      --
      testCase "Print Let" $
        runEval (eval (Let "x" (Print "foo" (CstInt 2)) (Print "bar" (CstInt 3))))
          @?= (["foo: 2", "bar: 3"], Right (ValInt 3)),
      --
      testCase "Print Unknown Variable" $
        runEval (eval (Let "x" (Print "foo" (CstInt 2)) (Print "bar" (Var "unknown"))))
          @?= (["foo: 2"], Left "Unknown variable: unknown"),
      --
      testCase "Print Nested" $
        runEval (eval (Print "outer" (Print "inner" (CstInt 2))))
          @?= (["inner: 2", "outer: 2"], Right (ValInt 2)),
      --
      testCase "Print Error Case" $
        runEval (eval (Let "x" (CstInt 2) (Print "error" (Div (CstInt 1) (CstInt 0)))))
          @?= ([], Left "Division by zero"),
       
      -- New test cases for KvPut and KvGet
      testCase "KvPut and KvGet" $
        eval' (Let "x" (KvPut (CstInt 0) (CstBool True)) (KvGet (CstInt 0)))
          @?= Right (ValBool True),

      --Error handling
      testCase "KvGet non-existent key" $
        eval' (Let "x" (KvPut (CstInt 0) (CstBool True)) (KvGet (CstInt 1)))
          @?= Left "Invalid key: ValInt 1",

      --
      testCase "KvPut replace value" $
        eval' (Let "x" (KvPut (CstInt 0) (CstBool True))
                       (Let "y" (KvPut (CstInt 0) (CstBool False))
                       (KvGet (CstInt 0))))
          @?= Right (ValBool False),

      --
      testCase "KvPut complex key" $
        eval' (Let "x" (KvPut (CstBool True) (CstInt 42)) 
                       (KvGet (CstBool True)))
          @?= Right (ValInt 42),

      --Error handling
      testCase "KvGet unknown key" $
        eval' (Let "x" (KvPut (CstInt 10) (CstBool True)) 
                       (KvGet (CstInt 5)))
          @?= Left "Invalid key: ValInt 5",

      --
      testCase "KvPut with a key-value pair" $
        eval' (Let "x" (KvPut (CstInt 1) (CstBool False)) 
                       (KvGet (CstInt 1)))
          @?= Right (ValBool False)
       
    ]

tests :: TestTree
tests = testGroup "APL" [evalTests]
