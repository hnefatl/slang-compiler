module Interpreters
(
    Program,
    Interpreter,
    Result(..),
    ResultConvertible,
    convert,
    runInterpreter,
    execInterpreter
) where

import qualified Interpreters.Ast as A

import Common
import Lexer (Position)
import Parser (parse)
import TypeChecker (typecheck)

type Program = String
type Interpreter v = A.Ast Position -> IO (Either Error v)

class ResultConvertible a where
    convert :: a -> Maybe Result

data Result = Unit
            | Integer Integer
            | Boolean Bool
            | Pair Result Result
            | Inl Result
            | Inr Result
            deriving Eq

instance ResultConvertible Result where
    convert = Just . id

instance Show Result where
    show Unit = "()"
    show (Integer i) = show i
    show (Boolean b) = if b then "true" else "false"
    show (Pair l r) = "(" ++ show l ++ ", " ++ show r ++ ")"
    show (Inl v) = "inl " ++ show v
    show (Inr v) = "inr " ++ show v

runInterpreter :: (Show a, ResultConvertible a) => Interpreter a -> Program -> IO (Either Error Result)
runInterpreter interpreter program = do
            let val = do -- Either monad
                    parsed <- parse program
                    _ <- typecheck parsed
                    return parsed
            case val of
                Left e -> return $ Left e
                Right parsed -> do -- IO monad
                    output <- interpreter (A.translate parsed)
                    case output of
                        Left err     -> return $ Left err
                        Right value -> case convert value of
                                Nothing     -> return $ Left ("Invalid result from program: " ++ show value)
                                Just result -> return $ Right result

execInterpreter :: (Show a, ResultConvertible a) => Interpreter a -> Program -> IO ()
execInterpreter interpreter program = do
            output <- runInterpreter interpreter program
            case output of
                Left e  -> putStrLn e
                Right v -> case convert v of
                        Nothing -> putStrLn ("Output of program wasn't a valid Result: " ++ show v)
                        Just x  -> print x