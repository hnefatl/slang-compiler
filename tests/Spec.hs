import Test.Tasty

import Test.Lexer.Lexer

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "slang-compiler" $
    [
        lexerTests
    ]