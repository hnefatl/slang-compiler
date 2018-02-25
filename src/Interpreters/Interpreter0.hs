-- Don't generate warnings if we discard the results of performing monadic actions
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Interpreters.Interpreter0
(
    interpret,
    Value(..),
    Error
) where

import Control.Monad (mapM, liftM, liftM2)
import System.IO (hFlush, stdout)
import System.Random (randomIO)

import Common
import qualified Interpreters.Ast as A
import qualified Interpreters as I (Result(..), ResultConvertible, convert)
import Interpreters.Interpreter0Monad

data Value = Unit
           | Integer Integer
           | Boolean Bool
           | Ref A.Variable
           | Pair Value Value
           | Inl Value
           | Inr Value
           | Fun A.Variable A.Ast
           deriving (Eq, Show)

instance I.ResultConvertible Value where
    convert Unit        = Just $ I.Unit
    convert (Integer i) = Just $ I.Integer i
    convert (Boolean b) = Just $ I.Boolean b
    convert (Ref _)     = Nothing
    convert (Pair l r)  = liftM2 I.Pair (I.convert l) (I.convert r)
    convert (Inl v)     = liftM I.Inl (I.convert v)
    convert (Inr v)     = liftM I.Inr (I.convert v)
    convert (Fun _ _)   = Nothing

type SlangInterpreter0 a = Interpreter0 A.Variable Value Error a

interpret :: A.Ast -> IO (Either Error Value)
interpret = runInterpreter0Monad . interpret'

interpret' :: A.Ast -> SlangInterpreter0 Value
interpret'  A.Unit               = return Unit
interpret' (A.Integer i)         = return $ Integer i
interpret' (A.Boolean b)         = return $ Boolean b
interpret' (A.Variable v)        = getValue' v
interpret' (A.Deref e)           = do
                                    Ref inner <- interpret' e
                                    getValue' inner
interpret' (A.Ref e)             = do
                                    inner <- interpret' e
                                    makeRef inner
interpret' (A.Pair l r)          = do
                                    lv <- (interpret' l)
                                    rv <- (interpret' r)
                                    return $ Pair lv rv
interpret' (A.UnaryOp op e)      = do
                                    val <- (interpret' e)
                                    interpretUOp op val
interpret' (A.BinaryOp op l r)   = do
                                    lv <- (interpret' l)
                                    rv <- (interpret' r)
                                    interpretBOp op lv rv
interpret' (A.Sequence es)       = do
                                    exprs <- mapM interpret' es
                                    return (last exprs)
interpret' (A.If c l r)          = do
                                    Boolean b <- interpret' c
                                    if b then interpret' l
                                    else interpret' r
interpret' (A.Inl e)             = do
                                    v <- interpret' e
                                    return (Inl v)
interpret' (A.Inr e)             = do
                                    v <- interpret' e
                                    return (Inr v)
interpret' (A.Case e l r)        = do
                                    ev <- interpret' e
                                    case ev of
                                        Inl v   -> do
                                                    lf <- interpret' l
                                                    apply lf v
                                        Inr v   -> do
                                                    rf <- interpret' r
                                                    apply rf v
                                        _       -> runtimeError ("Compiler Error: " ++ show e ++ " should be Inl or Inr")
interpret' (A.Fst e)             = do
                                    Pair l _ <- interpret' e
                                    return l
interpret' (A.Snd e)             = do
                                    Pair _ r <- interpret' e
                                    return r
interpret' orig@(A.While c e)    = do
                                    Boolean cond <- interpret' c
                                    if cond then do
                                        interpret' e
                                        interpret' orig
                                    else
                                        return Unit
interpret' (A.Let n e1 e2)       = do
                                    v <- interpret' e1
                                    local n v (interpret' e2)
interpret' (A.LetFun n f e)      = do
                                    fv <- interpret' f
                                    local n fv (interpret' e) 
interpret' (A.Fun x e)           = return $ Fun x e
interpret' (A.Application e1 e2) = do
                                    -- Evaluate e2 first, for compatibility with Tim Griffin's slang compiler
                                    x <- interpret' e2
                                    f <- interpret' e1
                                    apply f x
interpret' A.Input               = do
                                    liftIO (putStr "Input> ")
                                    liftIO (hFlush stdout) -- Output is line-buffered, so explicitly flush
                                    v <- liftIO readLn
                                    return (Integer v)

interpretUOp :: A.UOp -> Value -> SlangInterpreter0 Value
interpretUOp A.Neg (Integer i) = return $ Integer (-i)
interpretUOp A.Not (Boolean b) = return $ Boolean (not b)
interpretUOp op v = runtimeError ("Compiler Error: " ++ show op ++ " on " ++ show v)

interpretBOp :: A.BOp -> Value -> Value -> SlangInterpreter0 Value
interpretBOp A.Add (Integer l) (Integer r) = return $ Integer (l + r)
interpretBOp A.Sub (Integer l) (Integer r) = return $ Integer (l - r)
interpretBOp A.Mul (Integer l) (Integer r) = return $ Integer (l * r)
interpretBOp A.Div (Integer _) (Integer 0) = runtimeError "Division by zero"
interpretBOp A.Div (Integer l) (Integer r) = return $ Integer (l `div` r)
interpretBOp A.And (Boolean l) (Boolean r) = return $ Boolean (l && r)
interpretBOp A.Or (Boolean l) (Boolean r) = return $ Boolean (l || r)
interpretBOp A.Equal l r = return $ Boolean (l == r)
interpretBOp A.Less (Integer l) (Integer r) = return $ Boolean (l < r)
interpretBOp A.Assign (Ref l) v = do setValue l v
                                     return Unit
interpretBOp op l r = runtimeError ("Compiler Error: " ++ show op ++ " on " ++ show l ++ " and " ++ show r)

-- A version of Interpreter0.getValue that has a standard error message
getValue' :: A.Variable -> SlangInterpreter0 Value
getValue' k = getValueE k ("Couldn't find variable with name " ++ show k ++ " in environment")

apply :: Value -> Value -> SlangInterpreter0 Value
apply (Fun v e) x = do
                        x' <- valueToAst x
                        let e' = updateVariable v x' e
                        local v x (interpret' e')
apply f x = runtimeError ("Compiler Error: apply shouldn't have been called on " ++ show f ++ " with " ++ show x)

-- This is messy, but I've not found a nicer way to do it - can't use lenses as Ast isn't Traversable
updateVariable :: A.Variable -> A.Ast -> A.Ast -> A.Ast
updateVariable var val tree = updateVariable' tree
    where
        updateVariable' a@(A.Variable n) = if n == var then val else a
        updateVariable' a@A.Unit = a
        updateVariable' a@(A.Integer _) = a
        updateVariable' a@(A.Boolean _) = a
        updateVariable' a@(A.Input) = a
        updateVariable' (A.Deref e) = A.Deref (updateVariable' e)
        updateVariable' (A.Ref e) = A.Ref (updateVariable' e)
        updateVariable' (A.Pair l r) = A.Pair (updateVariable' l) (updateVariable' r)
        updateVariable' (A.UnaryOp op e) = (A.UnaryOp op) (updateVariable' e)
        updateVariable' (A.BinaryOp op l r) = (A.BinaryOp op) (updateVariable' l) (updateVariable' r)
        updateVariable' (A.Sequence es) = A.Sequence (map updateVariable' es)
        updateVariable' (A.If c l r) = A.If (updateVariable' c) (updateVariable' l) (updateVariable' r)
        updateVariable' (A.Inl e) = A.Inl (updateVariable' e)
        updateVariable' (A.Inr e) = A.Inr (updateVariable' e)
        updateVariable' (A.Case e l r) = A.Case (updateVariable' e) (updateVariable' l) (updateVariable' r)
        updateVariable' (A.Fst e) = A.Fst (updateVariable' e)
        updateVariable' (A.Snd e) = A.Snd (updateVariable' e)
        updateVariable' (A.While c e) = A.While (updateVariable' c) (updateVariable' e)
        updateVariable' (A.Let v b e) = (A.Let v) (updateVariable' b) (updateVariable' e)
        updateVariable' (A.LetFun v b e) = (A.LetFun v) (updateVariable' b) (updateVariable' e)
        updateVariable' (A.Fun v e) = (A.Fun v) (updateVariable' e)
        updateVariable' (A.Application e1 e2) = A.Application (updateVariable' e1) (updateVariable' e2)

valueToAst :: Value -> SlangInterpreter0 A.Ast
valueToAst Unit = return A.Unit
valueToAst (Integer i) = return $ A.Integer i
valueToAst (Boolean b) = return $ A.Boolean b
valueToAst (Ref n) = liftM A.Ref (getValueE n "Compiler Error: Value of ref not found" >>= valueToAst)
valueToAst (Pair l r) = liftM2 A.Pair (valueToAst l) (valueToAst r)
valueToAst (Inl l) = liftM A.Inl (valueToAst l)
valueToAst (Inr r) = liftM A.Inr (valueToAst r)
valueToAst (Fun x e) = return $ A.Fun x e

makeRef :: Value -> SlangInterpreter0 Value
makeRef v = do
                refName <- makeUnusedRefName
                setValue refName v
                return (Ref refName)

makeUnusedRefName :: SlangInterpreter0 A.Variable
makeUnusedRefName = do
                        rand <- liftIO randomIO
                        let name = "$" ++ show (rand :: Int)
                        exists <- getValue name
                        case exists of
                            Nothing -> return name
                            Just _  -> makeUnusedRefName