-- Don't generate warnings if we discard the results of performing monadic actions
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Interpreters.Interpreter0
(
    interpret,
    Value(..),
    Error
) where

import Control.Monad (mapM)
import qualified Data.Map as M

import qualified Interpreters.Ast as A
import Interpreters.Interpreter0Monad

data Value = Unit
           | Integer Integer
           | Boolean Bool
           | Ref A.Variable
           | Pair Value Value
           | Inl Value
           | Inr Value
           | Fun A.Lambda
           deriving (Eq, Show)

type Error = String
type SlangInterpreter0 = Interpreter0 A.Variable Value Error Value

interpret :: A.Ast -> IO (Either Error Value)
interpret = runInterpreter0 . interpret'

interpret' :: A.Ast -> SlangInterpreter0
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
                                        Inl v   -> doApply l v
                                        Inr v   -> doApply r v
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
interpret' (A.LetFun n f e)      = local n (Fun f) (interpret' e) 
interpret' (A.LetRecFun n f e)   = local n (Fun f) (interpret' e) 
interpret' (A.Fun l)             = return $ Fun l
interpret' (A.Application e1 e2) = do
                                    -- Evaluate e2 first, for compatibility with Tim Griffin's slang compiler
                                    x <- interpret' e2
                                    (Fun f) <- interpret' e1
                                    doApply f x
interpret' A.Input               = do
                                    liftIO (putStrLn "Input> ")
                                    v <- liftIO readLn
                                    return (Integer v)

interpretUOp :: A.UOp -> Value -> SlangInterpreter0
interpretUOp A.Neg (Integer i) = return $ Integer (-i)
interpretUOp A.Not (Boolean b) = return $ Boolean (not b)
interpretUOp op v = runtimeError ("Compiler Error: " ++ show op ++ " on " ++ show v)

interpretBOp :: A.BOp -> Value -> Value -> SlangInterpreter0
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

-- A version of interpret'er0.getValue that has a standard error message
getValue' :: A.Variable -> SlangInterpreter0
getValue' k = getValueE k ("Couldn't find variable with name " ++ show k ++ " in environment")

makeRef :: Value -> SlangInterpreter0
makeRef v = do
                env <- getEnvironment
                let refName = makeUnusedRefName env
                setValue refName v
                return (Ref refName)

makeUnusedRefName :: M.Map A.Variable Value -> A.Variable
makeUnusedRefName m = "$" ++ (show $ M.size m)

doApply :: A.Lambda -> Value -> SlangInterpreter0
doApply (A.Lambda x e) y = local x y (interpret' e)