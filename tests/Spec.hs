import Test.Tasty

import Test.Lexer
import Test.Parser
import Test.TypeChecker
import qualified Test.Interpreter0 as I0
import Test.FileTests

main :: IO ()
main = do
        ts <- tests
        defaultMain ts

interpreters :: [InterpreterInfo]
interpreters = [("Interpreter 0", interpreterWrapper I0.interpret')]

tests :: IO TestTree
tests = do
            fTests <- fileTests interpreters
            return $ testGroup "slang-compiler"
                [
                    lexerTests,
                    parserTests,
                    typecheckerTests,
                    I0.interpreterTests,
                    fTests
                ]