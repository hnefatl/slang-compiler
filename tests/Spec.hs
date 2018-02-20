import Test.Tasty

import Test.Lexer
import Test.Parser
import Test.TypeChecker

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "slang-compiler" $
    [
        lexerTests,
        parserTests,
        typecheckerTests
    ]