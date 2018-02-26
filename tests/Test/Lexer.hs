module Test.Lexer where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Lexer
import Lexer.Tokens

import Test.Util

tokenise' :: String -> Maybe Token
tokenise' = either (const Nothing) Just . tokenise

testCase' :: String -> Token -> TestTree
testCase' s e = testCase s (tokenise' s @?= Just e)

testCaseInvert' :: String -> Token -> TestTree
testCaseInvert' s e = testCase s (tokenise' s ~?= Just e)

lexerTests :: TestTree
lexerTests = testGroup "Lexer" $
    [
        testCase' "(" LParen,
        testCase' ")" RParen,
        testCase' "," Comma,
        testCase' ":" Colon,
        testCase' ";" Semicolon,

        testCase' "+" Add,
        testCase' "-" Sub,
        testCase' "*" Mult,
        testCase' "/" Div,
        
        testCase' "=" Equal,
        testCase' "<" Less,
        testCase' "~" Not,
        testCase' "&&" And,
        testCase' "||" Or,

        testCase' "|" Pipe,
        testCase' "->" Arrow,
        
        testCase' "ref" Ref,
        testCase' ":=" Assign,
        testCase' "!" Deref,

        testCase' "inl" Inl,
        testCase' "inr" Inr,
        testCase' "case" Case,
        testCase' "of" Of,

        testCase' "fst" Fst,
        testCase' "snd" Snd,

        testCase' "if" If,
        testCase' "then" Then,
        testCase' "else" Else,
        
        testCase' "let" Let,
        testCase' "in" In,

        testCase' "fun" Fun,

        testCase' "begin" Begin,
        testCase' "end" End,

        testCase' "while" While,
        testCase' "do" Do,

        testCase' "()" Unit,
        testProperty "Integer" $ forAll integers (\(i,s) -> tokenise' s === Just (Integer i)),
        testCase' "true" (Boolean True),
        testCase' "false" (Boolean False),

        testCase' "?" Input,

        testProperty "Identifier" $ forAll identifiers (\i -> tokenise' i === Just (Identifier i)),

        testCase' "(* Hello *)" EOF,
        testCase' "(* foo (* bar *) baz *)" EOF,
        testCase' "(*\nHello\n*)" EOF,
        testCase' "(* foo \n(* bar \n*) baz \n*)" EOF,

        testCase' "int" IntType,
        testCase' "bool" BoolType,
        testCase' "unit" UnitType
    ]