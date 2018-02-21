module Interpreters.Ast
(
    Ast(..),
    BOp,
    UOp,
    Variable,
    Lambda,
    translate
) where

import qualified Parser.Expressions as E

-- This is the internal representation of the AST - it's a translated form of the AST that's parsed.
-- This lets us remove the SimpleExpr/Expr divide as well as remove some redundancy: eg. parsing 
-- an expression in brackets results in a SimpleExpr (Expr ___), when ideally we want it to just be
-- an Expr.

data BOp = Add | Sub | Mul | Div | And | Or | Equal | Less | Assign deriving (Eq, Show)
data UOp = Neg | Not  deriving (Eq, Show)

type Variable = String
data Lambda = Lambda Variable Ast deriving (Eq, Show)

data Ast = Unit
         | Integer Integer
         | Boolean Bool
         | Variable String
         | Deref Ast
         | Ref Ast
         | Pair Ast Ast
         | UnaryOp UOp Ast
         | BinaryOp BOp Ast Ast
         | Sequence [Ast]
         | If Ast Ast Ast
         | Inl Ast
         | Inr Ast
         | Case Ast Lambda Lambda
         | Fst Ast
         | Snd Ast
         | While Ast Ast
         | Let Variable Ast Ast
         | LetFun Variable Lambda Ast
         | LetRecFun Variable Lambda Ast
         | Fun Variable Ast
         | Application Ast Ast
         | Input
         deriving (Eq, Show)

translate :: E.Expr -> Ast
translate (E.UnaryOp o e) = UnaryOp (translateUOp o) (translate e)
translate (E.ArithBinaryOp o l r) = BinaryOp (translateArithBOp o) (translate l) (translate r)
translate (E.BoolBinaryOp o l r) = BinaryOp (translateBoolBOp o) (translate l) (translate r)
translate (E.BinaryOp o l r) = BinaryOp (translateBOp o) (translate l) (translate r)
translate (E.Sequence es) = Sequence (map translate es)
translate (E.If c l r) = If (translate c) (translate l) (translate r)
translate (E.Inl e _) = Inl (translate e)
translate (E.Inr e _) = Inr (translate e)
translate (E.Case e l r) = Case (translate e) (translateLambda l) (translateLambda r)
translate (E.Fst e) = Fst (translate e)
translate (E.Snd e) = Snd (translate e)
translate (E.While c e) = While (translate c) (translate e)
translate (E.Let n _ v e) = Let n (translate v) (translate e)
translate (E.LetFun n f _ e) = LetFun n (translateLambda f) (translate e)
translate (E.LetRecFun n f _ e) = LetRecFun n (translateLambda f) (translate e)
translate (E.Fun x _ e) = Fun x (translate e)
translate (E.Application f e) = Application (translate f) (translateSimpleExpr e)
translate (E.SimpleExpr e) = translateSimpleExpr e

translateSimpleExpr :: E.SimpleExpr -> Ast
translateSimpleExpr E.Unit = Unit
translateSimpleExpr (E.Integer i) = Integer i
translateSimpleExpr (E.Boolean b) = Boolean b
translateSimpleExpr (E.Identifier i) = Variable i
translateSimpleExpr (E.Deref e) = Deref (translateSimpleExpr e)
translateSimpleExpr (E.Ref e) = Ref (translateSimpleExpr e)
translateSimpleExpr (E.Pair l r) = Pair (translate l) (translate r)
translateSimpleExpr (E.Expr e) = translate e


translateLambda :: E.Expr -> Lambda
translateLambda (E.Fun x _ e) = Lambda x (translate e)
translateLambda e = error ("Compiler error: Got a " ++ show e ++ " where a lambda was expected.")

translateUOp :: E.UOp -> UOp
translateUOp E.OpNot = Not
translateUOp E.OpNeg = Neg

translateArithBOp :: E.ArithBOp -> BOp
translateArithBOp E.OpAdd = Add
translateArithBOp E.OpSub = Sub
translateArithBOp E.OpMul = Mul
translateArithBOp E.OpDiv = Div

translateBoolBOp :: E.BoolBOp -> BOp
translateBoolBOp E.OpAnd = And
translateBoolBOp E.OpOr = Or

translateBOp :: E.BOp -> BOp
translateBOp E.OpEqual = Equal
translateBOp E.OpLess = Less
translateBOp E.OpAssign = Assign