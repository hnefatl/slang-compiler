import Test.Tasty

import Test.Lexer
import Test.Parser
import Test.TypeChecker
import Test.Interpreter0

main :: IO ()
main = do
        ts <- tests
        defaultMain ts

tests :: IO TestTree
tests = do
            interp0FileTests <- interpreter0FileTests
            return $ testGroup "slang-compiler"
                [
                    lexerTests,
                    parserTests,
                    typecheckerTests,
                    interpreter0Tests,
                    interp0FileTests
                ]