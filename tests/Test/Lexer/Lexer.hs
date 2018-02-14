module Test.Lexer.Lexer where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Lexer.Lexer

lexerTests :: TestTree
lexerTests = testGroup "Lexer" $
    [
        testProperty "Integer" (\i -> tokenise (show i) == [Integer i]),
        testCase "Boolean True" $ tokenise "True" @?= [Boolean True],
        testCase "Boolean False" $ tokenise "False" @?= [Boolean False]
    ]