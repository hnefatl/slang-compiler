-- Don't generate warnings if we discard the results of performing monadic actions
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Interpreters.Interpreter0
(
    interpret
) where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad (mapM)
import qualified Data.Map as M

import qualified Interpreters.Ast as A

data Value = Unit
           | Integer Integer
           | Boolean Bool
           | Ref A.Variable
           | Product Value Value
           | Inl Value
           | Inr Value
           | Fun A.Lambda
           deriving (Eq, Show)

type Error = String
type Interpreter = ExceptT Error (State (M.Map A.Variable Value))

interpret :: A.Ast -> Interpreter Value
interpret A.Unit = return Unit
interpret (A.Integer i) = return $ Integer i
interpret (A.Boolean b) = return $ Boolean b
interpret (A.Variable v) = getValue v
interpret (A.Deref e) = do
                            Ref inner <- interpret e
                            getValue inner
interpret (A.Ref e) = do
                        inner <- interpret e
                        makeRef inner
interpret (A.Pair l r) = do
                            lv <- (interpret l)
                            rv <- (interpret r)
                            return $ Product lv rv
interpret (A.UnaryOp op e) = do
                                val <- (interpret e)
                                interpretUOp op val
interpret (A.BinaryOp op l r) = do
                                lv <- (interpret l)
                                rv <- (interpret r)
                                interpretBOp op lv rv
interpret (A.Sequence es) = do
                                exprs <- mapM interpret es
                                return (last exprs)
interpret (A.If c l r) = do
                            Boolean b <- interpret c
                            if b then interpret l
                            else interpret r
interpret (A.Inl e) = do
                        v <- interpret e
                        return (Inl v)
interpret (A.Inr e) = do
                        v <- interpret e
                        return (Inr v)
interpret (A.Case e l r) = do
                            ev <- interpret e
                            case ev of
                                Inl v   -> doApply l v
                                Inr v   -> doApply r v
                                _       -> throwError ("Compiler Error: " ++ show e ++ " should be Inl or Inr")
interpret (A.Fst e) = do
                        Product l _ <- interpret e
                        return l
interpret (A.Snd e) = do
                        Product _ r <- interpret e
                        return r
interpret orig@(A.While c e) = do
                                Boolean cond <- interpret c
                                if cond then do
                                    interpret e
                                    interpret orig
                                else
                                    return Unit
interpret (A.Let n e1 e2) = do
                                v <- interpret e1
                                inLocal n v (interpret e2)
interpret (A.LetFun n f e) = do
                                inLocal n (Fun f) (interpret e) 
interpret (A.LetRecFun n f e) = do
                                inLocal n (Fun f) (interpret e) 
interpret (A.Application e1 e2) = do
                                    -- Interpret e2 first, for compatibility with Tim Griffin's slang compiler
                                    x <- interpret e2
                                    (Fun f) <- interpret e1
                                    doApply f x
interpret A.Input = error "STUB"
interpret _ = undefined

interpretUOp :: A.UOp -> Value -> Interpreter Value
interpretUOp A.Neg (Integer i) = return $ Integer (-i)
interpretUOp A.Not (Boolean b) = return $ Boolean (not b)
interpretUOp op v = throwError ("Compiler Error: " ++ show op ++ " on " ++ show v)

interpretBOp :: A.BOp -> Value -> Value -> Interpreter Value
interpretBOp A.Add (Integer l) (Integer r) = return $ Integer (l + r)
interpretBOp A.Sub (Integer l) (Integer r) = return $ Integer (l - r)
interpretBOp A.Mul (Integer l) (Integer r) = return $ Integer (l * r)
interpretBOp A.Div (Integer _) (Integer 0) = throwError "Division by zero"
interpretBOp A.Div (Integer l) (Integer r) = return $ Integer (l `div` r)
interpretBOp A.And (Boolean l) (Boolean r) = return $ Boolean (l && r)
interpretBOp A.Or (Boolean l) (Boolean r) = return $ Boolean (l || r)
interpretBOp A.Equal l r = return $ Boolean (l == r)
interpretBOp A.Less (Integer l) (Integer r) = return $ Boolean (l < r)
interpretBOp A.Assign (Ref l) v = do setValue l v
                                     return Unit
interpretBOp op l r = throwError ("Compiler Error: " ++ show op ++ " on " ++ show l ++ " and " ++ show r)

makeRef :: Value -> Interpreter Value
makeRef v = do
                env <- lift get
                let refName = makeUnusedRefName env
                setValue refName v
                return Unit

makeUnusedRefName :: M.Map A.Variable Value -> A.Variable
makeUnusedRefName m = "$" ++ (show $ M.size m)

doApply :: A.Lambda -> Value -> Interpreter Value
doApply (A.Lambda x e) y = do
                inLocal x y (interpret e)

getValue :: A.Variable -> Interpreter Value
getValue name = do
                val <- lift (gets (M.lookup name))
                case val of
                    Just v  -> return v
                    Nothing -> throwError ("Couldn't find variable with name " ++ name ++ " in environment")

setValue :: A.Variable -> Value -> Interpreter ()
setValue n v = do
                lift (modify (M.insert n v))

inLocal :: A.Variable -> Value -> Interpreter a -> Interpreter a
inLocal n v a = ExceptT $ withState (M.insert n v) (runExceptT a)