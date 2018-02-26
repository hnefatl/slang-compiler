module TypeChecker
(
    inferType,
    typecheck,
    Error
) where

import qualified Parser.Types as T
import qualified Parser.Expressions as E
import Data.Map.Lazy as M

import TypeChecker.TypeCheckerMonad
import TypeChecker.Error

import Common

type SlangTypeChecker a = TypeChecker E.Variable T.Type a

inferType :: E.Expr Position -> SlangTypeChecker T.Type
inferType (E.SimpleExpr e)            = inferTypeSimple e
inferType (E.UnaryOp pos E.OpNeg e)       = restrictedInfer T.isInteger (Error pos NegationNonInteger) inferType e
inferType (E.UnaryOp pos E.OpNot e)       = restrictedInfer T.isBoolean (Error pos NotNonBoolean) inferType e
inferType (E.ArithBinaryOp pos _ l r)     = do
                                            restrictedInfer_ T.isInteger (Error pos ArithmeticOpBadLHS) inferType l
                                            restrictedInfer T.isInteger (Error pos ArithmeticOpBadRHS) inferType r
inferType (E.BoolBinaryOp pos _ l r)      = do
                                            restrictedInfer_ T.isBoolean (Error pos BooleanOpBadLHS) inferType l
                                            restrictedInfer T.isBoolean (Error pos BooleanOpBadRHS) inferType r
inferType (E.BinaryOp pos E.OpEqual l r)  = do
                                            lType <- inferType l
                                            restrictedInfer_ (== lType) (Error pos EqualOpMismatch) inferType r
                                            return T.Boolean
inferType (E.BinaryOp pos E.OpLess l r)   = do
                                            restrictedInfer_ T.isInteger (Error pos LessOpNonIntegerRHS) inferType r
                                            restrictedInfer_ T.isInteger (Error pos LessOpNonIntegerLHS) inferType l
                                            return T.Boolean
inferType (E.BinaryOp pos E.OpAssign l r) = do
                                            rType <- inferType r
                                            restrictedInfer_ (== T.Ref rType) (Error pos AssignmentMismatch) inferType l
                                            return T.Unit
inferType (E.Sequence pos es)             = inferType (last es)
inferType (E.If pos e1 e2 e3)             = do
                                            restrictedInfer_ T.isBoolean (Error pos IfCondMismatch) inferType e1
                                            tType <- inferType e2
                                            restrictedInfer (== tType) (Error pos IfBranchMismatch) inferType e3
inferType (E.Inl pos e rt)                = do
                                            lt <- inferType e
                                            return $ T.Union lt rt
inferType (E.Inr pos e lt)                = do
                                            rt <- inferType e
                                            return $ T.Union lt rt
inferType (E.Input pos)                   = return T.Integer
inferType (E.Case pos e fl fr)            = do   
                                            T.Union lArg rArg <- restrictedInfer T.isUnion (Error pos CaseNonUnion) inferType e
                                            T.Fun _ lRet <- restrictedInfer (== T.Fun lArg T.Any) (Error pos $ CaseLeftMismatch lArg) inferType fl
                                            restrictedInfer_ (== T.Fun rArg lRet) (Error pos $ CaseRightMismatch rArg lRet) inferType fr
                                            return lRet
inferType (E.Fst pos e)                   = do
                                            T.Product t _ <- restrictedInfer T.isProduct (Error pos FstNonProduct) inferType e
                                            return t
inferType (E.Snd pos e)                   = do
                                            T.Product _ t <- restrictedInfer T.isProduct (Error pos SndNonProduct) inferType e
                                            return t
inferType (E.While pos e1 e2)             = do
                                            restrictedInfer_ (T.isBoolean) (Error pos WhileCondMismatch) inferType e1
                                            inferType e2
inferType (E.Let pos v t e1 e2)           = do
                                            restrictedInfer_ (== t) (Error pos LetMismatch) inferType e1
                                            inModifiedEnv (M.insert v t) (inferType e2)
inferType (E.LetFun pos n f@(E.Fun fpos _ argT _) retT e) =
                                        inModifiedEnv (M.insert n funType) $ do
                                            restrictedInfer_ (== funType) (Error pos $ LetFunMismatch funType) inferType f
                                            inferType e
                                        where funType = T.Fun argT retT
inferType (E.LetFun pos _ _ _ _)          = typeError (Error pos LetFunNonFunction)
inferType (E.Fun pos v t e)               = do
                                            innerType <- inModifiedEnv (M.insert v t) (inferType e)
                                            return $ T.Fun t innerType
inferType (E.Application pos f x)         = do
                                            T.Fun fArg fRet <- restrictedInfer T.isFun (Error pos ApplicationMismatch) inferType f
                                            restrictedInfer_ (== fArg) (Error pos ApplicationNonFunction) inferTypeSimple x
                                            return fRet



inferTypeSimple :: E.SimpleExpr Position -> SlangTypeChecker T.Type
inferTypeSimple (E.Expr e) = inferType e
inferTypeSimple (E.Unit _)              = return T.Unit
inferTypeSimple (E.Integer _ _)         = return T.Integer
inferTypeSimple (E.Boolean _ _)         = return T.Boolean
inferTypeSimple (E.Identifier pos name) = do
                                        varType <- fromEnv (M.lookup name)
                                        case varType of
                                            Just t -> return t
                                            _      -> typeError (Error pos $ NoSuchVariable name)
inferTypeSimple (E.Ref _ e)           = do
                                        innerType <- inferTypeSimple e
                                        return (T.Ref innerType)
inferTypeSimple (E.Deref pos e)         = do
                                        T.Ref t <- restrictedInfer T.isRef (Error pos DereferencingNonReference) inferTypeSimple e
                                        return t
inferTypeSimple (E.Pair _ l r)        = do
                                        lType <- inferType l
                                        rType <- inferType r
                                        return (T.Product lType rType)

restrictedInfer :: (T.Type -> Bool) -> Error -> (a -> SlangTypeChecker T.Type) -> (a -> SlangTypeChecker T.Type)
restrictedInfer p err inferrer expr = do
                                t <- inferrer expr
                                if p t then return t
                                else typeError err
restrictedInfer_ :: (T.Type -> Bool) -> Error -> (a -> SlangTypeChecker T.Type) -> (a -> SlangTypeChecker ())
restrictedInfer_ p err i e = restrictedInfer p err i e >> return ()

typecheck :: E.Expr Position -> Either FrontEndError T.Type
typecheck e = runTypeChecker (inferType e)