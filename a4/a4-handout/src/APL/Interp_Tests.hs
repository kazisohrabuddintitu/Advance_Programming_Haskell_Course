module APL.Interp_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (eval)
import APL.InterpIO (runEvalIO)
import APL.InterpPure (runEval)
import APL.Monad
import APL.Util (captureIO)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

eval' :: Exp -> ([String], Either Error Val)
eval' = runEval . eval

evalIO' :: Exp -> IO (Either Error Val)
evalIO' = runEvalIO . eval


tests :: TestTree
tests = testGroup "Free monad interpreters" [pureTests, ioTests]

pureTests :: TestTree
pureTests =
  testGroup
    "Pure interpreter"
    [ testCase "localEnv" $
        runEval
          ( localEnv (const [("x", ValInt 1)]) $
              askEnv
          )
          @?= ([], Right [("x", ValInt 1)]),
      --
      testCase "Let" $
        eval' (Let "x" (Add (CstInt 2) (CstInt 3)) (Var "x"))
          @?= ([], Right (ValInt 5)),
      --
      testCase "Let (shadowing)" $
        eval'
          ( Let
              "x"
              (Add (CstInt 2) (CstInt 3))
              (Let "x" (CstBool True) (Var "x"))
          )
          @?= ([], Right (ValBool True)),
      --
      testCase "State" $
        runEval
          ( do
              putState [(ValInt 0, ValInt 1)]
              modifyState $ map (\(key, _) -> (key, ValInt 5))
              getState
          )
          @?= ([], Right [(ValInt 0, ValInt 5)]),

      --
      testCase "Print" $
        runEval (evalPrint "test")
          @?= (["test"], Right ()),
      --
      testCase "Error" $
        runEval
          ( do
              _ <- failure "Oh no!"
              evalPrint "test"
          )
          @?= ([], Left "Oh no!"),
      --
      testCase "Div0" $
        eval' (Div (CstInt 7) (CstInt 0))
          @?= ([], Left "Division by zero"),
      --
      testCase "TryCatch (error caught)" $
        eval' (TryCatch (Div (CstInt 7) (CstInt 0)) (CstInt 42))
          @?= ([], Right (ValInt 42)),
      --
      testCase "TryCatch (no error)" $
        eval' (TryCatch (Div (CstInt 6) (CstInt 2)) (CstInt 42))
          @?= ([], Right (ValInt 3)),
      --
      testCase "TryCatch (nested error)" $
        eval' (TryCatch (Div (CstInt 7) (CstInt 0)) (Div (CstInt 1) (CstInt 0)))
          @?= ([], Left "Division by zero"),
      --
      testCase "KeyValue" $
        eval' (Let "x" (KvPut (CstInt 0) (CstInt 2)) (KvGet (CstInt 0)))
          @?= ([], Right (ValInt 2)),
      --
      testCase "KeyValue (Invalid)" $
        eval' (Let "x" (KvPut (CstInt 0) (CstBool True)) (KvGet (CstInt 1)))
          @?= ([], Left "Invalid key: ValInt 1"),
      --
      testCase "KeyValue" $
        eval' (Let "x" (KvPut (CstInt 0) (CstBool True)) (Let "y" (KvPut (CstInt 0) (CstBool False)) (KvGet (CstInt 0))))
          @?= ([], Right (ValBool False)),

      -- Test for a successful transaction
      testCase "Transaction Success" $
        eval' (Let "trans" (KvPut (CstInt 0) (CstInt 42)) (KvGet (CstInt 0)))
          @?= ([], Right (ValInt 42)),

      -- Test for a failed transaction (e.g., invalid key)
      testCase "Transaction Failure" $
        runEval
          ( do
              let badPut = evalKvPut (ValInt 0) (ValBool False) >> failure "die"
              let get0 = KvGet (CstInt 0)
              transaction badPut >> eval get0
          )
          @?= ([],Left "Invalid key: ValInt 0"),

      -- Test for rollback functionality
      testCase "Transaction Rollback" $
        eval' (Let "trans" (KvPut (CstInt 0) (CstInt 99)) (KvGet (CstInt 0)))
          @?= ([], Right (ValInt 99))

    ]


ioTests :: TestTree
ioTests =
  testGroup
    "IO interpreter"
    [ testCase "print" $ do
        let s1 = "Lalalalala"
            s2 = "Weeeeeeeee"
        (out, res) <-
          captureIO [] $
            runEvalIO $ do
              evalPrint s1
              evalPrint s2
        (out, res) @?= ([s1, s2], Right ()),
        -- NOTE: This test will give a runtime error unless you replace the
        -- version of `eval` in `APL.Eval` with a complete version that supports
        -- `Print`-expressions. Uncomment at your own risk.
        testCase "print 2" $ do
           (out, res) <-
             captureIO [] $
               evalIO' $
                 Print "This is also 1" $
                   Print "This is 1" $
                     CstInt 1
           (out, res) @?= (["This is 1: 1", "This is also 1: 1"], Right $ ValInt 1),
      --
      testCase "TryCatch (divided by zero)" $ do
        (out, res) <-
          captureIO [] $
            evalIO' (TryCatch (Div (CstInt 10) (CstInt 0)) (CstInt 42))
        (out, res) @?= ([], Right $ ValInt 42),
      -- --
      testCase "TryCatch (Equaliser)" $ do
        (out, res) <-
          captureIO [] $
            evalIO' (TryCatch (Eql (CstInt 0) (CstBool True)) (Div (CstInt 10) (CstInt 0)))
        (out, res) @?= ([], Left "Division by zero"),
      --
      testCase "TryCatch (With print)" $ do
        (out, res) <-
          captureIO [] $
            evalIO' (TryCatch (Eql (Print "From error try" (CstInt 2)) (CstBool True)) (CstInt 42))
        (out, res) @?= (["From error try: 2"], Right $ ValInt 42),
      --
      testCase "KeyValue" $ do
        (_, res) <-
          captureIO ["ValInt 1"] $
            runEvalIO $
              Free $ KvGetOp (ValInt 0) $ \val -> pure val
        res @?= Right (ValInt 1),
      --
      testCase "KeyValue" $ do
        (out, res) <-
          captureIO [] $
            evalIO' (Let "x" (KvPut (CstInt 0) (CstBool True)) (KvGet (CstInt 0)))
        (out, res) @?= ([],Right (ValBool True)),
      --
      testCase "KeyValue" $ do
        (_, res) <-
          captureIO ["ValInt 25"] $
            evalIO' (Let "x" (KvPut (CstInt 0) (CstBool True)) (KvGet (CstInt 1)))
        res @?= Right (ValInt 25),
      --
      testCase "KeyValue" $ do
        (out, res) <-
          captureIO [] $
            evalIO' (Let "x" (KvPut (CstInt 0) (CstBool True)) (Let "y" (KvPut (CstInt 0) (CstBool False)) (KvGet (CstInt 0))))
        (out, res) @?= ([],Right (ValBool False)),

      -- Test for a successful transaction
      testCase "Transaction Success" $ do
        (out, res) <- captureIO [] $
          evalIO' (Let "trans" (KvPut (CstInt 0) (CstInt 42)) (KvGet (CstInt 0)))
        (out, res) @?= ([], Right (ValInt 42)),
        
      -- Test for rollback functionality
      testCase "Transaction Rollback" $ do
        (out, res) <- captureIO [] $
          evalIO' (Let "trans" (KvPut (CstInt 0) (CstInt 99)) (KvGet (CstInt 0)))
        (out, res) @?= ([], Right (ValInt 99))
    ]
