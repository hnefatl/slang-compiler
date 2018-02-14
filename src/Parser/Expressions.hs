module Parser.Expressions 
(
    UOp(..),
    BOp(..),
    Variable,
    ExprPos,
    pos,
    expr,
    Expr(..)
) where

import Parser.Types
import Lexer.Tokens (Position)

data UOp = Not | Neg deriving (Eq, Show)
data BOp = Add | Sub | Mul | Div | And | Or | Less | Equal deriving (Eq, Show)

type Variable = String

-- An expression with one free variable. Used in eg. case statements where an expression is expected to
-- take an extra argument
data Lambda = Lambda Variable Type Expr deriving (Eq, Show)

type ExprPos = (Expr, Position)

pos :: ExprPos -> Position
pos = snd

expr :: ExprPos -> Expr
expr = fst

data Expr   = UnaryOp UOp Expr
            | BinaryOp BOp Expr Expr

            | Sequence [Expr]

            | If Expr Expr Expr

            | Ref Expr
            | Assign Expr Expr
            | Deref Expr

            | Inl Expr
            | Inr Expr
            | Case Expr Lambda Lambda

            | Pair Expr Expr
            | Fst Expr
            | Snd Expr

            | While Expr Expr

            | Let Variable Type Expr Expr
            | LetFun Variable Type Lambda Expr
            | LetRecFun Variable Type Lambda Expr

            | Fn Lambda
            | Apply Lambda Expr

            | Unit 
            | Integer Integer
            | Boolean Bool
            deriving (Eq, Show)