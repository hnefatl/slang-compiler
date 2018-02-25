import Test.Tasty

import Test.Util
import Test.Lexer
import Test.Parser
import Test.TypeChecker
import Test.Interpreters
import Test.FileTests

import Common
import Interpreters
import qualified Interpreters.Interpreter0 as I0

main :: IO ()
main = do
        ts <- tests
        defaultMain ts

testConvert :: (Show v, ResultConvertible v) => IO (Either Error v) -> IO (Either Error Result)
testConvert a = do
        output <- a
        case output of
            Left e  -> return $ Left e
            Right v -> case convert v of
                        Nothing -> return $ Left ("Program output wasn't a valid result. Got: " ++ show v)
                        Just r  -> return $ Right r

interpreters :: [InterpreterInfo Result]
interpreters = [("Interpreter 0", testConvert . I0.interpret)]

tests :: IO TestTree
tests = do
            fTests <- fileTests interpreters
            return $ testGroup "slang-compiler"
                [
                    lexerTests,
                    parserTests,
                    typecheckerTests,
                    interpreterTests interpreters,
                    fTests
                ]