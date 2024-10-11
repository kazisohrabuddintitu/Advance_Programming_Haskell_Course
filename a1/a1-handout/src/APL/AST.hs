module APL.AST
  ( VName,
    Exp (..),
    printExp,
  )
where

type VName = String

data Exp
  = CstInt Integer
  | CstBool Bool
  | Add Exp Exp
  | Sub Exp Exp
  | Mul Exp Exp
  | Div Exp Exp
  | Pow Exp Exp
  | Eql Exp Exp
  | If Exp Exp Exp
  | Var VName
  | Let VName Exp Exp
  -- TODO: add cases
  | Lambda VName Exp
  | Apply Exp Exp 
  | TryCatch Exp Exp
  deriving (Eq, Show)

printExp :: Exp -> String
printExp (CstInt z) = show z --converting to string representation of value of x
printExp (CstBool b) = show b --converting to string representation of value of b
printExp (Var vname) = vname
printExp (Add exp1 exp2) = "(" ++ printExp exp1 ++ " + " ++ printExp exp2 ++ ")"
printExp (Sub exp1 exp2) = "(" ++ printExp exp1 ++ " - " ++ printExp exp2 ++ ")"
printExp (Mul exp1 exp2) = "(" ++ printExp exp1 ++ " * " ++ printExp exp2 ++ ")"
printExp (Div exp1 exp2) = "(" ++ printExp exp1 ++ " / " ++ printExp exp2 ++ ")"
printExp (Pow exp1 exp2) = "(" ++ printExp exp1 ++ " ** " ++ printExp exp2 ++ ")"
printExp (Eql exp1 exp2) = "(" ++ printExp exp1 ++ " == " ++ printExp exp2 ++ ")"
printExp (If cond exp1 exp2) = "(if " ++ printExp cond ++ " then " ++ printExp exp1 ++ " else " ++ printExp exp2 ++ ")"
printExp (Let vname exp1 exp2) = "(let " ++ vname ++ " = " ++ printExp exp1 ++ " in " ++ printExp exp2 ++ ")"
printExp (Lambda vname exp) = "(\\ " ++ vname ++ " -> " ++ printExp exp ++ ")"
printExp (Apply exp1 exp2) = "(" ++ printApply exp1 ++ " " ++ printArg exp2 ++ ")"
printExp (TryCatch exp1 exp2) = "(try " ++ printExp exp1 ++ " catch " ++ printExp exp2 ++ ")"

--parentheses for any argument of an Apply unless that argument is a constant or variable
printArg :: Exp -> String
printArg e@(CstInt _) = printExp e
printArg e@(CstBool _) = printExp e
printArg e@(Var _) = printExp e
printArg e = "(" ++ printExp e ++ ")"  -- Parentheses for everything else


--parentheses for any function in an Apply unless it is a constant, variable or another apply
printApply :: Exp -> String
printApply e@(CstInt _) = printExp e
printApply e@(CstBool _) = printExp e
printApply e@(Var _) = printExp e
printApply e@(Lambda _ _) = "(" ++ printExp e ++ ")"
printApply e@(Apply _ _) = printExp e
printApply e = "(" ++ printExp e ++ ")"  -- Parentheses for everything else
