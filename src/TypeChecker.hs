-- Don't generate warnings if we discard the result of a monadic action
-- Necessary as eg. restrictedInfer either throws an exception or returns the matching type
-- If it doesn't throw an exception, we may not need the return value as we know it 
-- passed the restriction imposed by the function.
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module TypeChecker
(
    inferType,
    typecheck,
    Error
) where

import qualified Parser.Types as T
import qualified Parser.Expressions as E

import TypeChecker.TypeCheckerMonad

import Data.Map.Lazy as M

type Error = String

type SlangTypeChecker = TypeChecker E.Variable T.Type Error T.Type

inferType :: E.Expr -> SlangTypeChecker
inferType (E.SimpleExpr e)            = inferTypeSimple e
inferType (E.UnaryOp E.OpNeg e)       = restrictedInfer T.isInteger "Negation of a non-integer expression" inferType e
inferType (E.UnaryOp E.OpNot e)       = restrictedInfer T.isBoolean "\"Not\"-ing of a non-boolean expression" inferType e
inferType (E.ArithBinaryOp _ l r)     = do
                                        restrictedInfer T.isInteger "LHS of arithmetic operator is non-integer" inferType l
                                        restrictedInfer T.isInteger "RHS of arithmetic operator is non-integer" inferType r
inferType (E.BoolBinaryOp _ l r)      = do
                                        restrictedInfer T.isBoolean "LHS of boolean operator is non-boolean" inferType l
                                        restrictedInfer T.isBoolean "RHS of boolean operator is non-boolean" inferType r
inferType (E.BinaryOp E.OpEqual l r)  = do
                                        lType <- inferType l
                                        restrictedInfer (== lType) "Type of LHS doesn't match type of RHS in equality" inferType r
                                        return T.Boolean
inferType (E.BinaryOp E.OpLess l r)   = do
                                        restrictedInfer T.isInteger "RHS of < must have type integer" inferType r
                                        restrictedInfer T.isInteger "LHS of < must have type integer" inferType l
                                        return T.Boolean
inferType (E.BinaryOp E.OpAssign l r) = do
                                        rType <- inferType r
                                        restrictedInfer (== T.Ref rType) "Type of LHS doesn't match type of RHS in assignment" inferType l
                                        return T.Unit
inferType (E.Sequence es)             = inferType (last es)
inferType (E.If e1 e2 e3)             = do
                                        restrictedInfer T.isBoolean "Condition of if-statement is non-boolean" inferType e1
                                        tType <- inferType e2
                                        restrictedInfer (== tType) "Branches of if-statement don't have the same type" inferType e3
inferType (E.Inl e (T.Union lt rt))   = do
                                        restrictedInfer (== lt) "Expression in inl must match left side of union type" inferType e
                                        return $ T.Union lt rt
inferType (E.Inl _ _)                 = typeError "Expected union-type in inl"
inferType (E.Inr e (T.Union lt rt))   = do
                                        restrictedInfer (== rt) "Expression in inr must match right side of union type" inferType e
                                        return $ T.Union lt rt
inferType (E.Inr _ _)                 = typeError "Expected union-type in inr"
inferType (E.Case e fl fr)            = do   
                                         T.Union lArg rArg <- restrictedInfer T.isUnion "Expression in case statement must have type union" inferType e
                                         T.Fun _ lRet <- restrictedInfer (== T.Fun lArg T.Any) ("Expression in inl branch of case statement must have type " ++ show lArg ++ " -> *") inferType fl
                                         restrictedInfer (== T.Fun rArg lRet) ("Expression in inr branch of case statement must have type " ++ show rArg ++ " -> " ++ show lRet) inferType fr
                                         return lRet
inferType (E.Fst e)                   = do
                                         T.Product t _ <- restrictedInfer T.isProduct "Expression in fst statement must have product type" inferType e
                                         return t
inferType (E.Snd e)                   = do
                                         T.Product _ t <- restrictedInfer T.isProduct "Expression in snd statement must have product type" inferType e
                                         return t
inferType (E.While e1 e2)             = do
                                         restrictedInfer (T.isBoolean) "Condition in while loop must have boolean type" inferType e1
                                         inferType e2
inferType (E.Let v t e1 e2)           = do
                                         restrictedInfer (== t) "Variable's initialiser must have the same type as the variable" inferType e1
                                         inModifiedEnv (M.insert v t) (inferType e2)
inferType (E.LetFun n f t e)          = do
                                        fType <- restrictedInfer (== T.Fun T.Any t) "Non-function provided in let fun statement" inferType f
                                        inModifiedEnv (M.insert n fType) (inferType e)
inferType (E.LetRecFun n f@(E.Fun _ argT _) retT e) =
                                        inModifiedEnv (M.insert n funType) $ do
                                            restrictedInfer (== funType) ("Expression in let rec fun statement isn't of type " ++ show funType) inferType f
                                            inferType e
                                        where funType = T.Fun argT retT
inferType (E.LetRecFun _ _ _ _)       = typeError "Expected fun type in let rec fun statement."
inferType (E.Fun v t e)               = do innerType <- inModifiedEnv (M.insert v t) (inferType e)
                                           return $ T.Fun t innerType
inferType (E.Application f x)         = do
                                        T.Fun fArg fRet <- restrictedInfer (T.isFun) "Expected function in application" inferType f
                                        restrictedInfer (== fArg) "Function applied to value of wrong type" inferTypeSimple x
                                        return fRet



inferTypeSimple :: E.SimpleExpr -> SlangTypeChecker
inferTypeSimple (E.Expr e) = inferType e
inferTypeSimple E.Unit              = return T.Unit
inferTypeSimple (E.Integer _)       = return T.Integer
inferTypeSimple (E.Boolean _)       = return T.Boolean
inferTypeSimple (E.Identifier name) = do
                                        varType <- fromEnv (M.lookup name)
                                        case varType of
                                            Just t -> return t
                                            _      -> typeError ("Missing variable in environment " ++ name)
inferTypeSimple (E.Ref e)           = do
                                        innerType <- inferTypeSimple e
                                        return (T.Ref innerType)
inferTypeSimple (E.Deref e)         = do
                                        T.Ref t <- restrictedInfer T.isRef "Dereferencing a non-reference" inferTypeSimple e
                                        return t
inferTypeSimple (E.Pair l r)        = do
                                        lType <- inferType l
                                        rType <- inferType r
                                        return (T.Product lType rType)

restrictedInfer :: (T.Type -> Bool) -> Error -> (a -> SlangTypeChecker) -> (a -> SlangTypeChecker)
restrictedInfer p err inferrer expr = do
                                t <- inferrer expr
                                if p t then return t
                                else typeError err

typecheck :: E.Expr -> Either Error T.Type
typecheck e = runTypeChecker (inferType e)