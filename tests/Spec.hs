import Test.Tasty

import Test.Lexer.Lexer
import Test.Parser.Parser

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "slang-compiler" $
    [
        lexerTests,
        parserTests
    ]