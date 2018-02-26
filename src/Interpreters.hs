module Interpreters
(
    Program,
    Interpreter,
    Result(..),
    ResultConvertible,
    convertResult,
    convertInterpreterError,
    convertInterpreterResult,
    runInterpreter,
    execInterpreter
) where

import qualified Interpreters.Ast as A

import Common
import Parser (parse)
import TypeChecker (typecheck)

type Program = String
type Interpreter e v = A.Ast Position -> IO (Either e v)

class ResultConvertible a where
    convertResult :: a -> Maybe Result

data Result = Unit
            | Integer Integer
            | Boolean Bool
            | Pair Result Result
            | Inl Result
            | Inr Result
            deriving Eq

instance ResultConvertible Result where
    convertResult = Just . id

instance Show Result where
    show Unit = "()"
    show (Integer i) = show i
    show (Boolean b) = if b then "true" else "false"
    show (Pair l r) = "(" ++ show l ++ ", " ++ show r ++ ")"
    show (Inl v) = "inl " ++ show v
    show (Inr v) = "inr " ++ show v

-- Modify an interpreter to produce a front-end error rather than an internal one
convertInterpreterError :: ErrorConvertible e => Interpreter e v -> Interpreter FrontEndError v
convertInterpreterError i ast = do
                result <- i ast
                case result of
                    Left e  -> return $ Left (convertError e)
                    Right v -> return $ Right v

-- Modify an interpreter to produce a standard result rather than an internal value
convertInterpreterResult :: (Show v, ResultConvertible v) => Interpreter FrontEndError v -> Interpreter FrontEndError Result
convertInterpreterResult i ast = do
        output <- i ast
        case output of
            Left e  -> return $ Left e
            Right v -> case convertResult v of
                        Nothing -> return $ Left ("Invalid result from program: " ++ show v)
                        Just r  -> return $ Right r

-- Run an interpreter on a program, produce an error or a result
runInterpreter :: (Show a, ResultConvertible a) => Interpreter FrontEndError a -> Program -> IO (Either FrontEndError Result)
runInterpreter interpreter program = do
            let val = do -- Either monad
                    parsed <- parse program
                    _ <- typecheck parsed
                    return parsed
            case val of
                Left e -> return $ Left e
                Right parsed -> do -- IO monad
                    output <- (convertInterpreterResult interpreter) (A.translate parsed)
                    return output

-- Fully front-end, run an interpreter and print the output
execInterpreter :: (Show a, ResultConvertible a) => Interpreter FrontEndError a -> Program -> IO ()
execInterpreter interpreter program = do
            output <- runInterpreter interpreter program
            case output of
                Left e  -> putStrLn e
                Right v -> case convertResult v of
                        Nothing -> putStrLn ("Output of program wasn't a valid Result: " ++ show v)
                        Just x  -> print x