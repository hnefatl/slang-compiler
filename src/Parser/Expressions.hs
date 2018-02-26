{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Parser.Expressions
(
    UOp(..),
    ArithBOp(..),
    BoolBOp(..),
    BOp(..),
    Variable,
    SimpleExpr(..),
    Expr(..),
    stripExprState,
    stripSimpleExprState
) where

import Parser.Types (Type)

data UOp = OpNot | OpNeg deriving (Eq, Show)
data ArithBOp = OpAdd | OpSub | OpMul | OpDiv deriving (Eq, Show)
data BoolBOp = OpAnd | OpOr deriving (Eq, Show)
data BOp = OpEqual | OpLess | OpAssign deriving (Eq, Show)

type Variable = String

data SimpleExpr a = Unit a
                  | Integer a Integer
                  | Boolean a Bool
                  | Identifier a String
                  | Deref a (SimpleExpr a)
                  | Ref a (SimpleExpr a)
                  | Pair a (Expr a) (Expr a)
                  | Expr (Expr a)
                  deriving (Eq, Show, Functor, Foldable, Traversable)

data Expr a = UnaryOp a UOp (Expr a)
            | ArithBinaryOp a ArithBOp (Expr a) (Expr a)
            | BoolBinaryOp a BoolBOp (Expr a) (Expr a)
            | BinaryOp a BOp (Expr a) (Expr a)

            | Sequence a [Expr a]

            | If a (Expr a) (Expr a) (Expr a) 

            | Inl a (Expr a) Type  -- inl bool 5
            | Inr a (Expr a) Type  -- inr int true
            | Case a (Expr a) (Expr a) (Expr a) -- case inl bool 5 of (inl x : int) -> true | (inr x : bool) -> x end

            | Fst a (Expr a)
            | Snd a (Expr a)

            | While a (Expr a) (Expr a)

            | Let a Variable Type (Expr a) (Expr a)  -- let (x : int) = 1 in ... end
            | LetFun a Variable (Expr a) Type (Expr a)  -- let f(x : int) : int = x + 1 in ... end. The first Expr is a function.

            | Fun a Variable Type (Expr a)
            | Application a (Expr a) (SimpleExpr a)

            | Input a

            | SimpleExpr (SimpleExpr a)
            deriving (Eq, Show, Functor, Foldable, Traversable)
        
stripExprState :: Expr a -> Expr ()
stripExprState = fmap (const ())
        
stripSimpleExprState :: SimpleExpr a -> SimpleExpr ()
stripSimpleExprState = fmap (const ())