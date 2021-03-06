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
inferType (E.SimpleExpr e)                = inferTypeSimple e
inferType (E.UnaryOp pos E.OpNeg e)       = restrictedInfer inferType e T.isInteger (Error pos $ OpNegNonInteger e)
inferType (E.UnaryOp pos E.OpNot e)       = restrictedInfer inferType e T.isBoolean (Error pos $ OpNotNonBoolean e)
inferType (E.ArithBinaryOp pos _ l r)     = do
                                            restrictedInfer_ inferType l T.isInteger (Error pos $ ArithmeticOpBadLHS l)
                                            restrictedInfer inferType r T.isInteger (Error pos $ ArithmeticOpBadRHS r)
inferType (E.BoolBinaryOp pos _ l r)      = do
                                            restrictedInfer_ inferType l T.isBoolean (Error pos $ BooleanOpBadLHS l)
                                            restrictedInfer inferType r T.isBoolean (Error pos $ BooleanOpBadRHS r)
inferType (E.BinaryOp pos E.OpEqual l r)  = do
                                            lType <- inferType l
                                            restrictedInfer_ inferType r (== lType) (Error pos $ EqualOpMismatch l r)
                                            return T.Boolean
inferType (E.BinaryOp pos E.OpLess l r)   = do
                                            restrictedInfer_ inferType r T.isInteger (Error pos $ LessOpNonIntegerRHS r)
                                            restrictedInfer_ inferType l T.isInteger (Error pos $ LessOpNonIntegerLHS l)
                                            return T.Boolean
inferType (E.BinaryOp pos E.OpAssign l r) = do
                                            rType <- inferType r
                                            restrictedInfer_ inferType l (== T.Ref rType) (Error pos $ AssignmentMismatch l r)
                                            return T.Unit
inferType (E.Sequence _ es)               = inferType (last es)
inferType (E.If pos e1 e2 e3)             = do
                                            restrictedInfer_ inferType e1 T.isBoolean (Error pos $ IfCondMismatch e1)
                                            tType <- inferType e2
                                            restrictedInfer inferType e3 (== tType) (Error pos $ IfBranchMismatch e2 e3)
inferType (E.Inl _ e rt)                  = do
                                            lt <- inferType e
                                            return $ T.Union lt rt
inferType (E.Inr _ e lt)                  = do
                                            rt <- inferType e
                                            return $ T.Union lt rt
inferType (E.Input _)                     = return T.Integer
inferType (E.Case pos e fl fr)            = do   
                                            T.Union lArg rArg <- restrictedInfer inferType e T.isUnion (Error pos $ CaseNonUnion e)
                                            T.Fun _ lRet <- restrictedInfer inferType fl (== T.Fun lArg T.Any) (Error pos $ CaseLeftMismatch lArg fl)
                                            restrictedInfer_ inferType fr (== T.Fun rArg lRet) (Error pos $ CaseRightMismatch rArg fr)
                                            return lRet
inferType (E.Fst pos e)                   = do
                                            T.Product t _ <- restrictedInfer inferType e T.isProduct (Error pos $ FstNonProduct e)
                                            return t
inferType (E.Snd pos e)                   = do
                                            T.Product _ t <- restrictedInfer inferType e T.isProduct (Error pos $ SndNonProduct e)
                                            return t
inferType (E.While pos e1 e2)             = do
                                            restrictedInfer_ inferType e1 (T.isBoolean) (Error pos $ WhileCondMismatch e1)
                                            inferType e2
inferType (E.Let pos v t e b)             = do
                                            restrictedInfer_ inferType e (== t) (Error pos $ LetMismatch t e)
                                            inModifiedEnv (M.insert v t) (inferType b)
inferType (E.LetFun pos n f@(E.Fun _ _ argT _) retT e) =
                                            inModifiedEnv (M.insert n funType) $ do
                                                restrictedInfer_ inferType f (== funType) (Error pos $ LetFunMismatch funType f)
                                                inferType e
                                            where funType = T.Fun argT retT
inferType (E.LetFun pos _ e _ _)          = typeError (Error pos $ LetFunNonFunction e)
inferType (E.Fun _ v t e)                 = do
                                            innerType <- inModifiedEnv (M.insert v t) (inferType e)
                                            return $ T.Fun t innerType
inferType (E.Application pos f x)         = do
                                            T.Fun fArg fRet <- restrictedInfer inferType f T.isFun (Error pos $ ApplicationMismatch f x)
                                            restrictedInfer_ inferTypeSimple x (== fArg) (Error pos $ ApplicationBadArg fArg x)
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
inferTypeSimple (E.Ref _ e)             = do
                                            innerType <- inferTypeSimple e
                                            return (T.Ref innerType)
inferTypeSimple (E.Deref pos e)         = do
                                            T.Ref t <- restrictedInfer inferTypeSimple e T.isRef (Error pos $ DereferencingNonReference e)
                                            return t
inferTypeSimple (E.Pair _ l r)          = do
                                            lType <- inferType l
                                            rType <- inferType r
                                            return (T.Product lType rType)

restrictedInfer :: (a -> SlangTypeChecker T.Type) -> a -> (T.Type -> Bool) -> Error -> SlangTypeChecker T.Type
restrictedInfer inferrer expr p err = do
                                t <- inferrer expr
                                if p t then return t
                                else typeError err
restrictedInfer_ :: (a -> SlangTypeChecker T.Type) -> a -> (T.Type -> Bool) -> Error -> SlangTypeChecker ()
restrictedInfer_ i e p err = restrictedInfer i e p err >> return ()

typecheck :: E.Expr Position -> Either FrontEndError T.Type
typecheck e = runTypeChecker (inferType e)