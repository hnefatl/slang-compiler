{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
-- Don't warn us about the missing signatures for all the "stateless*" utility methods
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Interpreters.Ast
(
    Ast(..),
    UOp(..),
    BOp(..),
    Variable,
    translate,
    stripState,
    statelessUnit,
    statelessInteger,
    statelessBoolean,
    statelessVariable,
    statelessDeref,
    statelessRef,
    statelessPair,
    statelessUnaryOp,
    statelessBinaryOp,
    statelessSequence,
    statelessIf,
    statelessInl,
    statelessInr,
    statelessCase,
    statelessFst,
    statelessSnd,
    statelessWhile,
    statelessLet,
    statelessLetFun,
    statelessFun,
    statelessApplication,
    statelessInput
) where

import qualified Parser.Expressions as E

-- This is the internal representation of the AST - it's a translated form of the AST that's parsed.
-- This lets us remove the SimpleExpr/Expr divide as well as remove some redundancy: eg. parsing 
-- an expression in brackets results in a SimpleExpr (Expr ___), when ideally we want it to just be
-- an Expr.

data UOp = Neg | Not  deriving (Eq, Show)
data BOp = Add | Sub | Mul | Div | And | Or | Equal | Less | Assign deriving (Eq, Show)

type Variable = String

data Ast a = Unit a
           | Integer a Integer
           | Boolean a Bool
           | Variable a Variable
           | Deref a (Ast a)
           | Ref a (Ast a)
           | Pair a (Ast a) (Ast a)
           | UnaryOp a UOp (Ast a)
           | BinaryOp a BOp (Ast a) (Ast a)
           | Sequence a [(Ast a)]
           | If a (Ast a) (Ast a) (Ast a)
           | Inl a (Ast a)
           | Inr a (Ast a)
           | Case a (Ast a) (Ast a) (Ast a)
           | Fst a (Ast a)
           | Snd a (Ast a)
           | While a (Ast a) (Ast a)
           | Let a Variable (Ast a) (Ast a)
           | LetFun a Variable (Ast a) (Ast a)
           | Fun a Variable (Ast a)
           | Application a (Ast a) (Ast a)
           | Input a
           deriving (Eq, Show, Functor, Foldable, Traversable)

translate :: E.Expr a -> Ast a
translate (E.UnaryOp a o e) = UnaryOp a (translateUOp o) (translate e)
translate (E.ArithBinaryOp a o l r) = BinaryOp a (translateArithBOp o) (translate l) (translate r)
translate (E.BoolBinaryOp a o l r) = BinaryOp a (translateBoolBOp o) (translate l) (translate r)
translate (E.BinaryOp a o l r) = BinaryOp a (translateBOp o) (translate l) (translate r)
translate (E.Sequence a es) = Sequence a (map translate es)
translate (E.If a c l r) = If a (translate c) (translate l) (translate r)
translate (E.Inl a e _) = Inl a (translate e)
translate (E.Inr a e _) = Inr a (translate e)
translate (E.Case a e l r) = Case a (translate e) (translate l) (translate r)
translate (E.Fst a e) = Fst a (translate e)
translate (E.Snd a e) = Snd a (translate e)
translate (E.While a c e) = While a (translate c) (translate e)
translate (E.Let a n _ v e) = Let a n (translate v) (translate e)
translate (E.LetFun a n f _ e) = LetFun a n (translate f) (translate e)
translate (E.Fun a x _ e) = Fun a x (translate e)
translate (E.Application a f e) = Application a (translate f) (translateSimpleExpr e)
translate (E.Input a) = Input a
translate (E.SimpleExpr e) = translateSimpleExpr e

translateSimpleExpr :: E.SimpleExpr a -> Ast a
translateSimpleExpr (E.Unit a) = Unit a
translateSimpleExpr (E.Integer a i) = Integer a i
translateSimpleExpr (E.Boolean a b) = Boolean a b
translateSimpleExpr (E.Identifier a i) = Variable a i
translateSimpleExpr (E.Deref a e) = Deref a (translateSimpleExpr e)
translateSimpleExpr (E.Ref a e) = Ref a (translateSimpleExpr e)
translateSimpleExpr (E.Pair a l r) = Pair a (translate l) (translate r)
translateSimpleExpr (E.Expr e) = translate e


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


stripState :: Ast a -> Ast ()
stripState = fmap (const ())

-- Duplicates of all the Ast constructors but without the state
statelessUnit = Unit ()
statelessInteger = Integer ()
statelessBoolean = Boolean ()
statelessVariable = Variable ()
statelessDeref = Deref ()
statelessRef = Ref ()
statelessPair = Pair ()
statelessUnaryOp = UnaryOp ()
statelessBinaryOp = BinaryOp ()
statelessSequence = Sequence ()
statelessIf = If ()
statelessInl = Inl ()
statelessInr = Inr ()
statelessCase = Case ()
statelessFst = Fst ()
statelessSnd = Snd ()
statelessWhile = While ()
statelessLet = Let ()
statelessLetFun = LetFun ()
statelessFun = Fun ()
statelessApplication = Application ()
statelessInput = Input ()