-- Don't generate warnings if we discard the result of a monadic action
-- Necessary as eg. restrictedInfer either throws an exception or returns the matching type
-- If it doesn't throw an exception, we may not need the return value as we know it 
-- passed the restriction imposed by the function.
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module TypeChecker.TypeChecker
(
    inferType,
    typecheck
) where

import qualified Parser.Types as T
import qualified Parser.Expressions as E

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Data.Map.Lazy as M

type TypeChecker = ExceptT Error (Reader (M.Map E.Variable T.Type)) T.Type
type Error = String

inferType :: E.Expr -> TypeChecker
inferType (E.SimpleExpr e) = inferTypeSimple e
inferType (E.UnaryOp E.OpNeg e)     = restrictedInfer T.isInteger "Negation of a non-integer expression" inferType e
inferType (E.UnaryOp E.OpNot e)     = restrictedInfer T.isBoolean "\"Not\"-ing of a non-boolean expression" inferType e
inferType (E.ArithBinaryOp _ l r)  = do
                                        restrictedInfer T.isInteger "LHS of arithmetic operator is non-integer" inferType l
                                        restrictedInfer T.isInteger "RHS of arithmetic operator is non-integer" inferType r
inferType (E.BoolBinaryOp _ l r)  = do
                                        restrictedInfer T.isBoolean "LHS of boolean operator is non-boolean" inferType l
                                        restrictedInfer T.isBoolean "RHS of boolean operator is non-boolean" inferType r
inferType (E.BinaryOp E.OpEqual l r) = do
                                        lType <- inferType l
                                        restrictedInfer (== lType) "Type of LHS doesn't match type of RHS in equality" inferType r
inferType (E.BinaryOp E.OpAssign l r) = do
                                        rType <- inferType r
                                        restrictedInfer (== T.Ref rType) "Type of LHS doesn't match type of RHS in assignment" inferType l
                                        return T.Unit
inferType (E.Sequence es)             = inferType (last es)
inferType (E.If e1 e2 e3)             = do
                                        restrictedInfer T.isBoolean "Condition of if-statement is non-boolean" inferType e1
                                        tType <- inferType e2
                                        restrictedInfer (== tType) "Branches of if-statement don't have the same type" inferType e3
inferType (E.Inl e (T.Union lt _))   = restrictedInfer (== lt) "Expression in inl must match left side of union type" inferType e
inferType (E.Inl _ _)                = throwE "Expected union-type in inl"
inferType (E.Inr e (T.Union _ rt))   = restrictedInfer (== rt) "Expression in inr must match right side of union type" inferType e
inferType (E.Inr _ _)                = throwE "Expected union-type in inr"
inferType (E.Case e (E.Lambda _ lt le) (E.Lambda _ rt re)) = do
                                       restrictedInfer (== T.Union lt rt) "Expression in case statement must have union type matching the union of the branch argument types" inferType e
                                       exprType <- inferType le
                                       restrictedInfer (== exprType) "Lambda types in case statement must match" inferType re
inferType (E.Fst e)                  = do
                                        (T.Product t _) <- restrictedInfer (T.isProduct) "Expression in fst statement must have product type" inferType e
                                        return t
inferType (E.Snd e)                  = do
                                        (T.Product _ t) <- restrictedInfer (T.isProduct) "Expression in snd statement must have product type" inferType e
                                        return t
inferType (E.While e1 e2)            = do
                                        restrictedInfer (T.isBoolean) "Condition in while loop must have boolean type" inferType e1
                                        inferType e2
--inferType (E.Let v e1 e2)            = do




inferTypeSimple :: E.SimpleExpr -> TypeChecker
inferTypeSimple (E.Expr e) = inferType e
inferTypeSimple E.Unit              = return T.Unit
inferTypeSimple (E.Integer _)       = return T.Integer
inferTypeSimple (E.Boolean _)       = return T.Boolean
inferTypeSimple (E.Identifier name) = do
                                        varType <- lift $ asks (M.lookup name)
                                        case varType of
                                            Just t -> return t
                                            _      -> throwE ("Missing variable in environment " ++ name)
inferTypeSimple (E.Ref e)           = do
                                        innerType <- inferTypeSimple e
                                        return (T.Ref innerType)
inferTypeSimple (E.Deref e)         = restrictedInfer T.isRef "Dereferencing a non-reference" inferTypeSimple e
inferTypeSimple (E.Pair l r)        = do
                                        lType <- inferType l
                                        rType <- inferType r
                                        return (T.Product lType rType)

restrictedInfer :: (T.Type -> Bool) -> Error -> (a -> TypeChecker) -> (a -> TypeChecker)
restrictedInfer p err inferrer expr = do
                                t <- inferrer expr
                                if p t then return t
                                else throwE err


typecheck :: E.Expr -> Maybe String
typecheck = undefined