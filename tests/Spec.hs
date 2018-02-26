import Test.Tasty

import Test.Util
import Test.Lexer
import Test.Parser
import Test.TypeChecker
import Test.Interpreters
import Test.FileTests

import Interpreters
import qualified Interpreters.Interpreter0 as I0

main :: IO ()
main = do
        ts <- tests
        defaultMain ts


interpreters :: [InterpreterInfo Result]
interpreters = [("Interpreter 0", convertInterpreterResult $ convertInterpreterError I0.interpret)]

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