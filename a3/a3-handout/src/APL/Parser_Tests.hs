module APL.Parser_Tests (tests) where

import APL.AST (Exp (..))
import APL.Parser (parseAPL)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

parserTest :: String -> Exp -> TestTree
parserTest s e =
  testCase s $
    case parseAPL "input" s of
      Left err -> assertFailure err
      Right e' -> e' @?= e

parserTestFail :: String -> TestTree
parserTestFail s =
  testCase s $
    case parseAPL "input" s of
      Left _ -> pure ()
      Right e ->
        assertFailure $
          "Expected parse error but received this AST:\n" ++ show e

tests :: TestTree
tests =
  testGroup
    "Parsing"
    [ testGroup
        "Constants"
        [ parserTest "123" $ CstInt 123,
          parserTest " 123" $ CstInt 123,
          parserTest "123 " $ CstInt 123,
          parserTestFail "123f",
          parserTest "true" $ CstBool True,
          parserTest "false" $ CstBool False
        ],
      testGroup
        "Basic operators"
        [ parserTest "x+y" $ Add (Var "x") (Var "y"),
          parserTest "x-y" $ Sub (Var "x") (Var "y"),
          parserTest "x*y" $ Mul (Var "x") (Var "y"),
          parserTest "x/y" $ Div (Var "x") (Var "y"),
          parserTest "x==y" $ Eql (Var "x") (Var "y")
        ],
      testGroup
        "Operator priority"
        [ parserTest "x+y+z" $ Add (Add (Var "x") (Var "y")) (Var "z"),
          parserTest "x+y-z" $ Sub (Add (Var "x") (Var "y")) (Var "z"),
          parserTest "x+y*z" $ Add (Var "x") (Mul (Var "y") (Var "z")),
          parserTest "x*y*z" $ Mul (Mul (Var "x") (Var "y")) (Var "z"),
          parserTest "x/y/z" $ Div (Div (Var "x") (Var "y")) (Var "z")
        ],
      testGroup
        "Conditional expressions"
        [ parserTest "if x then y else z" $ If (Var "x") (Var "y") (Var "z"),
          parserTest "if x then y else if x then y else z" $
            If (Var "x") (Var "y") $
              If (Var "x") (Var "y") (Var "z"),
          parserTest "if x then (if x then y else z) else z" $
            If (Var "x") (If (Var "x") (Var "y") (Var "z")) (Var "z"),
          parserTest "1 + if x then y else z" $
            Add (CstInt 1) (If (Var "x") (Var "y") (Var "z"))
        ],
      testGroup
        "Lexing edge cases"
        [ parserTest "2 " $ CstInt 2,
          parserTest " 2" $ CstInt 2
        ],
      testGroup
        "Function Application"
        [ parserTest "f x" $ Apply (Var "f") (Var "x"),
          parserTest "f x y" $ Apply (Apply (Var "f") (Var "x")) (Var "y"),
          parserTest "x y z" $ Apply (Apply (Var "x") (Var "y")) (Var "z"),
          parserTest "x (y z)" $ Apply (Var "x") (Apply (Var "y") (Var "z")),
          parserTest "f (g x)" $ Apply (Var "f") (Apply (Var "g") (Var "x")),
          parserTestFail "x if x then y else z"
        ],
      testGroup
        "Equality and Power Operators"
        [ parserTest "x*y**z" $ Mul (Var "x") (Pow (Var "y") (Var "z")),
          parserTest "x+y==y+x" $ Eql (Add (Var "x") (Var "y")) (Add (Var "y") (Var "x")),
          parserTest "x**y**z" $ Pow (Var "x") (Pow (Var "y") (Var "z"))
        ],
      testGroup
        "Print, Put, and Get operations"
        [ parserTest "put x y" $ KvPut (Var "x") (Var "y"),
          parserTest "get x + y" $ Add (KvGet (Var "x")) (Var "y"),
          parserTest "getx" $ Var "getx",
          parserTest "print \"foo\" x" $ Print "foo" (Var "x"),
          parserTest "print \"\" x" $ Print "" (Var "x"),
          parserTest "put 1 2" $ KvPut (CstInt 1) (CstInt 2),
          parserTest "get a + put b c" $ Add (KvGet (Var "a")) (KvPut (Var "b") (Var "c")),
          parserTestFail "put", 
          parserTestFail "print x", 
          parserTestFail "get ", 
          parserTestFail "put x", 
          parserTestFail "print \"missing closing quote x\""
        ],
      testGroup
        "Lambda, Let, TryCatch"
        [ parserTest "\\x -> x + 1" $ Lambda "x" (Add (Var "x") (CstInt 1)),
          parserTestFail "let \\x -> x in z",
          parserTest "let x = y in z" $ Let "x" (Var "y") (Var "z"),
          parserTestFail "let true = y in z",
          parserTest "try x catch y" $ TryCatch (Var "x") (Var "y"),
          parserTestFail "try 1/0 catch"
        ]
    ]
