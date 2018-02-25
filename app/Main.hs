module Main where

import System.Environment (getArgs)
import Control.Monad (when)

import Options
import Interpreters
import qualified Interpreters.Interpreter0 as I0

main :: IO ()
main = do
    args <- getArgs
    case parseOptions args of
        Left err -> putStrLn err
        Right opt ->
            case _file opt of
                Nothing   -> putStrLn "A file to be interpreted must be given"
                Just path -> do
                    program <- readFile path
                    when (_interpreter0 opt) $ execInterpreter I0.interpret program