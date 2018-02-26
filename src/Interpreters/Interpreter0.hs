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
import Lexer (Position)
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
           | Fun A.Variable (A.Ast Position)
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

interpret :: A.Ast Position -> IO (Either Error Value)
interpret = runInterpreter0Monad . interpret'

interpret' :: A.Ast Position -> SlangInterpreter0 Value
interpret' (A.Unit _)               = return Unit
interpret' (A.Integer _ i)         = return $ Integer i
interpret' (A.Boolean _ b)         = return $ Boolean b
interpret' (A.Variable _ v)        = getValue' v
interpret' (A.Deref _ e)           = do
                                    Ref inner <- interpret' e
                                    getValue' inner
interpret' (A.Ref _ e)             = do
                                    inner <- interpret' e
                                    makeRef inner
interpret' (A.Pair _ l r)          = do
                                    lv <- (interpret' l)
                                    rv <- (interpret' r)
                                    return $ Pair lv rv
interpret' (A.UnaryOp _ op e)      = do
                                    val <- (interpret' e)
                                    interpretUOp op val
interpret' (A.BinaryOp _ op l r)   = do
                                    lv <- (interpret' l)
                                    rv <- (interpret' r)
                                    interpretBOp op lv rv
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
interpret' (A.Case _ e l r)        = do
                                    ev <- interpret' e
                                    case ev of
                                        Inl v   -> do
                                                    lf <- interpret' l
                                                    apply lf v
                                        Inr v   -> do
                                                    rf <- interpret' r
                                                    apply rf v
                                        _       -> runtimeError ("Compiler Error: " ++ show e ++ " should be Inl or Inr")
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
interpret' (A.Application _ e1 e2) = do
                                    -- Evaluate e2 first, for compatibility with Tim Griffin's slang compiler
                                    x <- interpret' e2
                                    f <- interpret' e1
                                    apply f x
interpret' (A.Input _)               = do
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
updateVariable :: A.Variable -> (Position -> A.Ast Position) -> A.Ast Position -> A.Ast Position
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

valueToAst :: Value -> SlangInterpreter0 (Position -> A.Ast Position)
valueToAst Unit = return (\p -> A.Unit p)
valueToAst (Integer i) = return $ \p -> A.Integer p i
valueToAst (Boolean b) = return $ \p -> A.Boolean p b
valueToAst (Ref n) = do
                        value <- getValueE n "Compiler Error: Value of ref not found"
                        inner <- valueToAst value
                        return $ \p -> A.Ref p (inner p)
valueToAst (Pair l r) = do
                            lt <- valueToAst l
                            rt <- valueToAst r
                            return $ \p -> A.Pair p (lt p) (rt p)
valueToAst (Inl l) = do
                        inner <- valueToAst l
                        return $ \p -> A.Inl p (inner p)
valueToAst (Inr r) = do
                        inner <- valueToAst r
                        return $ \p -> A.Inr p (inner p)
valueToAst (Fun x e) = return $ \p -> A.Fun p x e


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