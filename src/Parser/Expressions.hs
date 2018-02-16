module Parser.Expressions
(
    UOp(..),
    ArithBOp(..),
    BoolBOp(..),
    BOp(..),
    Variable,
    Lambda(..),
    SimpleExpr(..),
    Expr(..)
) where

import Parser.Types

data UOp = OpNot | OpNeg deriving (Eq, Show)
data ArithBOp = OpAdd | OpSub | OpMul | OpDiv | OpLess deriving (Eq, Show)
data BoolBOp = OpAnd | OpOr deriving (Eq, Show)
data BOp = OpEqual | OpAssign deriving (Eq, Show)

type Variable = String

-- An expression with one free variable. Used in eg. case statements where an expression is expected to
-- take an extra argument
data Lambda = Lambda Variable Type Expr deriving (Eq, Show)

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

            | Inl Type Expr
            | Inr Type Expr
            | Case Expr Lambda Lambda

            | Fst Expr
            | Snd Expr

            | While Expr Expr

            | Let Variable Type Expr Expr
            | LetFun Variable Lambda Type Expr
            | LetRecFun Variable Lambda Type Expr

            | Fun Lambda
            | Apply Expr SimpleExpr

            | SimpleExpr SimpleExpr
            deriving (Eq, Show)