module Test.Lexer.Lexer where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Lexer.Lexer

lexerTests :: TestTree
lexerTests = testGroup "Lexer" $
    [
        testCase "LParen" $ tokenise "(" @?= [LParen],
        testCase "RParen" $ tokenise ")" @?= [RParen],
        testCase "Comma" $ tokenise "," @?= [Comma],
        testCase "Colon" $ tokenise ":" @?= [Colon],
        testCase "Semicolon" $ tokenise ";" @?= [Semicolon],

        testCase "Add" $ tokenise "+" @?= [Add],
        testCase "Sub" $ tokenise "-" @?= [Sub],
        testCase "Mult" $ tokenise "*" @?= [Mult],
        testCase "Div" $ tokenise "/" @?= [Div],
        
        testCase "Equal" $ tokenise "=" @?= [Equal],
        testCase "Less" $ tokenise "<" @?= [Less],
        testCase "Not" $ tokenise "~" @?= [Not],
        testCase "And" $ tokenise "&&" @?= [And],
        testCase "Or" $ tokenise "||" @?= [Or],

        testCase "Pipe" $ tokenise "|" @?= [Pipe],
        testCase "Arrow" $ tokenise "->" @?= [Arrow],
        
        testCase "Ref" $ tokenise "ref" @?= [Ref],
        testCase "Assign" $ tokenise ":=" @?= [Assign],
        testCase "Deref" $ tokenise "!" @?= [Deref],

        testCase "Inl" $ tokenise "inl" @?= [Inl],
        testCase "Inr" $ tokenise "inr" @?= [Inr],
        testCase "Case" $ tokenise "case" @?= [Case],
        testCase "Of" $ tokenise "of" @?= [Of],

        testCase "Fst" $ tokenise "fst" @?= [Fst],
        testCase "Snd" $ tokenise "snd" @?= [Snd],

        testCase "If" $ tokenise "if" @?= [If],
        testCase "Then" $ tokenise "then" @?= [Then],
        testCase "Else" $ tokenise "else" @?= [Else],
        
        testCase "Let" $ tokenise "let" @?= [Let],
        testCase "In" $ tokenise "in" @?= [In],

        testCase "Fun" $ tokenise "fun" @?= [Fun],

        testCase "Begin" $ tokenise "begin" @?= [Begin],
        testCase "End" $ tokenise "end" @?= [End],

        testCase "While" $ tokenise "while" @?= [While],
        testCase "Do" $ tokenise "do" @?= [Do],

        testCase "Unit" $ tokenise "()" @?= [Unit],
        testProperty "Integer" (\i -> tokenise (show i) == [Integer i]),
        testCase "Boolean True" $ tokenise "true" @?= [Boolean True],
        testCase "Boolean False" $ tokenise "false" @?= [Boolean False]
    ]