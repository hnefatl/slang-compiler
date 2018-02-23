module Interpreters
(
    runInterpreter0
) where

import qualified Interpreters.Ast as A
import qualified Interpreters.Interpreter0 as I0

import Common
import Parser (parse)
import TypeChecker (typecheck)

runInterpreter :: Show a => String -> (A.Ast -> IO (Either Error a)) -> IO (Either Error a)
runInterpreter program interpreter = do
            let val = do
                    parsed <- parse program
                    _ <- typecheck parsed
                    return parsed
            case val of
                Left e -> return $ Left e
                Right parsed -> do
                    output <- interpreter (A.translate parsed)
                    return output

runInterpreter0 :: String -> IO ()
runInterpreter0 prog = do
            val <- runInterpreter prog I0.interpret
            case val of
                Left e  -> error e
                Right v -> print v