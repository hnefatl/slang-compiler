module Parser.Expressions
(
    UOp(..),
    ArithBOp(..),
    BoolBOp(..),
    BOp(..),
    Variable,
    SimpleExpr(..),
    Expr(..)
) where

import Parser.Types

data UOp = OpNot | OpNeg deriving (Eq, Show)
data ArithBOp = OpAdd | OpSub | OpMul | OpDiv | OpLess deriving (Eq, Show)
data BoolBOp = OpAnd | OpOr deriving (Eq, Show)
data BOp = OpEqual | OpAssign deriving (Eq, Show)

type Variable = String

data SimpleExpr = Unit
                | Integer Integer
                | Boolean Bool
                | Identifier String
                | Deref SimpleExpr
                | Ref SimpleExpr
                | Pair Expr Expr
                | Expr Expr
                deriving (Eq, Show)

data Expr   = UnaryOp UOp Expr
            | ArithBinaryOp ArithBOp Expr Expr
            | BoolBinaryOp BoolBOp Expr Expr
            | BinaryOp BOp Expr Expr

            | Sequence [Expr]

            | If Expr Expr Expr

            | Inl Expr Type  -- inl 5    : int + bool
            | Inr Expr Type  -- inr true : int + bool
            | Case Expr Expr Expr -- case inl 5 : int + bool of (inl x : int) -> true | (inr x : bool) -> x end

            | Fst Expr
            | Snd Expr

            | While Expr Expr

            | Let Variable Type Expr Expr  -- let (x : int) = 1 in ... end
            | LetFun Variable Expr Type Expr  -- let f(x : int) : int = x + 1 in ... end
            | LetRecFun Variable Expr Type Expr  -- let rec f(x : int) : int = f(x - 1) in ... end

            | Fun Variable Type Expr
            | Apply Expr SimpleExpr

            | SimpleExpr SimpleExpr
            deriving (Eq, Show)