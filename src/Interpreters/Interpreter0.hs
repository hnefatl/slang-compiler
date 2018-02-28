-- Don't generate warnings if we discard the results of performing monadic actions
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Interpreters.Interpreter0
(
    interpret,
    Value(..),
    Error
) where

import Control.Monad (mapM)
import System.IO (hFlush, stdout)
import System.Random (randomIO)

import Common
import Interpreters.Error
import qualified Interpreters.Ast as A
import Interpreters (Interpreter)
import Interpreters.Interpreter0.Monad
import Interpreters.Interpreter0.Values

interpret :: Interpreter Error Value
interpret = runInterpreter0Monad . interpret'

interpret' :: A.Ast Position -> Interpreter0 Value
interpret' (A.Unit _)              = return Unit
interpret' (A.Integer _ i)         = return $ Integer i
interpret' (A.Boolean _ b)         = return $ Boolean b
interpret' (A.Variable pos v)      = getValue' pos v
interpret' (A.Deref pos e)         = do
                                        Ref inner <- interpret' e
                                        getValue' pos inner
interpret' (A.Ref _ e)             = do
                                        inner <- interpret' e
                                        makeRef inner
interpret' (A.Pair _ l r)          = do
                                        lv <- (interpret' l)
                                        rv <- (interpret' r)
                                        return $ Pair lv rv
interpret' (A.UnaryOp pos op e)    = do
                                        val <- (interpret' e)
                                        interpretUOp pos op val
interpret' (A.BinaryOp pos op l r) = do
                                        lv <- (interpret' l)
                                        rv <- (interpret' r)
                                        interpretBOp pos op lv rv
interpret' (A.Sequence _ es)       = do
                                        exprs <- mapM interpret' es
                                        return (last exprs)
interpret' (A.If _ c l r)          = do
                                        Boolean b <- interpret' c
                                        if b then interpret' l
                                        else interpret' r
interpret' (A.Inl _ e)             = do
                                        v <- interpret' e
                                        return (Inl v)
interpret' (A.Inr _ e)             = do
                                        v <- interpret' e
                                        return (Inr v)
interpret' (A.Case pos e l r)      = do
                                        ev <- interpret' e
                                        case ev of
                                            Inl v -> do
                                                    lf <- interpret' l
                                                    apply pos lf v
                                            Inr v -> do
                                                    rf <- interpret' r
                                                    apply pos rf v
                                            _     -> runtimeError (Error pos $ FATAL_InlInrMismatch e)
interpret' (A.Fst _ e)             = do
                                        Pair l _ <- interpret' e
                                        return l
interpret' (A.Snd _ e)             = do
                                        Pair _ r <- interpret' e
                                        return r
interpret' orig@(A.While _ c e)    = do
                                        Boolean cond <- interpret' c
                                        if cond then do
                                            interpret' e
                                            interpret' orig
                                        else
                                            return Unit
interpret' (A.Let _ n e1 e2)       = do
                                        v <- interpret' e1
                                        local n v (interpret' e2)
interpret' (A.LetFun _ n f e)      = do
                                        fv <- interpret' f
                                        local n fv (interpret' e) 
interpret' (A.Fun _ x e)           = return $ Fun x e
interpret' (A.Application pos e1 e2) = do
                                        -- Evaluate e2 first, for compatibility with Tim Griffin's slang compiler
                                        x <- interpret' e2
                                        f <- interpret' e1
                                        apply pos f x
interpret' (A.Input _)               = do
                                        liftIO (putStr "Input> ")
                                        liftIO (hFlush stdout) -- Output is line-buffered, so explicitly flush
                                        v <- liftIO readLn
                                        return (Integer v)

interpretUOp :: Position -> A.UOp -> Value -> Interpreter0 Value
interpretUOp _ A.Neg (Integer i) = return $ Integer (-i)
interpretUOp _ A.Not (Boolean b) = return $ Boolean (not b)
interpretUOp pos op _ = runtimeError (Error pos $ FATAL_InvalidUOpArguments op)

interpretBOp :: Position -> A.BOp -> Value -> Value -> Interpreter0 Value
interpretBOp _   A.Add (Integer l) (Integer r)  = return $ Integer (l + r)
interpretBOp _   A.Sub (Integer l) (Integer r)  = return $ Integer (l - r)
interpretBOp _   A.Mul (Integer l) (Integer r)  = return $ Integer (l * r)
interpretBOp pos A.Div (Integer l) (Integer 0)  = runtimeError (Error pos $ DivisionByZero l)
interpretBOp _   A.Div (Integer l) (Integer r)  = return $ Integer (l `div` r)
interpretBOp _   A.And (Boolean l) (Boolean r)  = return $ Boolean (l && r)
interpretBOp _   A.Or  (Boolean l) (Boolean r)  = return $ Boolean (l || r)
interpretBOp _   A.Equal l r                    = return $ Boolean (l == r)
interpretBOp _   A.Less (Integer l) (Integer r) = return $ Boolean (l < r)
interpretBOp _   A.Assign (Ref n) v = setValue n v >> return Unit
interpretBOp pos op _ _ = runtimeError (Error pos $ FATAL_InvalidBOpArguments op)

-- A version of Interpreter0.getValue that has a standard error message
getValue' :: Position -> A.VariableName -> Interpreter0 Value
getValue' pos k = getValueE k (Error pos $ MissingVariable k)

apply :: Position -> Value -> Value -> Interpreter0 Value
apply pos (Fun v e) x = do
                        x' <- valueToAst pos x
                        let e' = updateVariable v x' e
                        local v x (interpret' e')
apply pos _ _ = runtimeError (Error pos FATAL_InvalidApplication)

-- This is messy, but I've not found a nicer way to do it
updateVariable :: A.VariableName -> (Position -> A.Ast Position) -> A.Ast Position -> A.Ast Position
updateVariable var valUpdater tree = updateVariable' tree
    where
        updateVariable' a@(A.Variable p n) = if n == var then valUpdater p else a
        updateVariable' a@(A.Unit _) = a
        updateVariable' a@(A.Integer _ _) = a
        updateVariable' a@(A.Boolean _ _) = a
        updateVariable' a@(A.Input _) = a
        updateVariable' (A.Deref p e) = A.Deref p (updateVariable' e)
        updateVariable' (A.Ref p e) = A.Ref p (updateVariable' e)
        updateVariable' (A.Pair p l r) = A.Pair p (updateVariable' l) (updateVariable' r)
        updateVariable' (A.UnaryOp p op e) = (A.UnaryOp p op) (updateVariable' e)
        updateVariable' (A.BinaryOp p op l r) = (A.BinaryOp p op) (updateVariable' l) (updateVariable' r)
        updateVariable' (A.Sequence p es) = A.Sequence p (map updateVariable' es)
        updateVariable' (A.If p c l r) = A.If p (updateVariable' c) (updateVariable' l) (updateVariable' r)
        updateVariable' (A.Inl p e) = A.Inl p (updateVariable' e)
        updateVariable' (A.Inr p e) = A.Inr p (updateVariable' e)
        updateVariable' (A.Case p e l r) = A.Case p (updateVariable' e) (updateVariable' l) (updateVariable' r)
        updateVariable' (A.Fst p e) = A.Fst p (updateVariable' e)
        updateVariable' (A.Snd p e) = A.Snd p (updateVariable' e)
        updateVariable' (A.While p c e) = A.While p (updateVariable' c) (updateVariable' e)
        updateVariable' (A.Let p v b e) = (A.Let p v) (updateVariable' b) (updateVariable' e)
        updateVariable' (A.LetFun p v b e) = (A.LetFun p v) (updateVariable' b) (updateVariable' e)
        updateVariable' (A.Fun p v e) = (A.Fun p v) (updateVariable' e)
        updateVariable' (A.Application p e1 e2) = A.Application p (updateVariable' e1) (updateVariable' e2)

-- We return a function from Position -> Ast so that the value we construct can be slotted into the converted
-- AST generated by updateVariable and point to the right place
-- The position argument to this function is in case an error occurs during the value -> ast partial conversion
valueToAst :: Position -> Value -> Interpreter0 (Position -> A.Ast Position)
valueToAst _    Unit       = return (\p -> A.Unit p)
valueToAst _   (Integer i) = return $ \p -> A.Integer p i
valueToAst _   (Boolean b) = return $ \p -> A.Boolean p b
valueToAst _   (Fun x e)   = return $ \p -> A.Fun p x e
valueToAst pos (Ref n)     = do
                                value <- getValueE n (Error pos FATAL_RefMissing)
                                inner <- valueToAst pos value
                                return $ \p -> A.Ref p (inner p)
valueToAst pos (Pair l r)  = do
                                lt <- valueToAst pos l
                                rt <- valueToAst pos r
                                return $ \p -> A.Pair p (lt p) (rt p)
valueToAst pos (Inl l)     = do
                                inner <- valueToAst pos l
                                return $ \p -> A.Inl p (inner p)
valueToAst pos (Inr r)     = do
                                inner <- valueToAst pos r
                                return $ \p -> A.Inr p (inner p)


makeRef :: Value -> Interpreter0 Value
makeRef v = do
                refName <- makeUnusedRefName
                setValue refName v
                return (Ref refName)

makeUnusedRefName :: Interpreter0 A.VariableName
makeUnusedRefName = do
                        rand <- liftIO randomIO
                        let name = "$" ++ show (rand :: Int)
                        exists <- getValue name
                        case exists of
                            Nothing -> return name
                            Just _  -> makeUnusedRefName