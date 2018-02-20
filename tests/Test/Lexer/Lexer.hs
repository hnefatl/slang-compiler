module Test.Lexer.Lexer where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Lexer.Lexer
import Lexer.Tokens

import Test.Util

tokenise' :: String -> Maybe TokenClass
tokenise' = either (const Nothing) (Just . fst) . tokenise

lexerTests :: TestTree
lexerTests = testGroup "Lexer" $
    [
        testCase "LParen" $ tokenise' "(" @?= Just LParen,
        testCase "RParen" $ tokenise' ")" @?= Just RParen,
        testCase "Comma" $ tokenise' "," @?= Just Comma,
        testCase "Colon" $ tokenise' ":" @?= Just Colon,
        testCase "Semicolon" $ tokenise' ";" @?= Just Semicolon,

        testCase "Add" $ tokenise' "+" @?= Just Add,
        testCase "Sub" $ tokenise' "-" @?= Just Sub,
        testCase "Mult" $ tokenise' "*" @?= Just Mult,
        testCase "Div" $ tokenise' "/" @?= Just Div,
        
        testCase "Equal" $ tokenise' "=" @?= Just Equal,
        testCase "Less" $ tokenise' "<" @?= Just Less,
        testCase "Not" $ tokenise' "~" @?= Just Not,
        testCase "And" $ tokenise' "&&" @?= Just And,
        testCase "Or" $ tokenise' "||" @?= Just Or,

        testCase "Pipe" $ tokenise' "|" @?= Just Pipe,
        testCase "Arrow" $ tokenise' "->" @?= Just Arrow,
        
        testCase "Ref" $ tokenise' "ref" @?= Just Ref,
        testCase "Assign" $ tokenise' ":=" @?= Just Assign,
        testCase "Deref" $ tokenise' "!" @?= Just Deref,

        testCase "Inl" $ tokenise' "inl" @?= Just Inl,
        testCase "Inr" $ tokenise' "inr" @?= Just Inr,
        testCase "Case" $ tokenise' "case" @?= Just Case,
        testCase "Of" $ tokenise' "of" @?= Just Of,

        testCase "Fst" $ tokenise' "fst" @?= Just Fst,
        testCase "Snd" $ tokenise' "snd" @?= Just Snd,

        testCase "If" $ tokenise' "if" @?= Just If,
        testCase "Then" $ tokenise' "then" @?= Just Then,
        testCase "Else" $ tokenise' "else" @?= Just Else,
        
        testCase "Let" $ tokenise' "let" @?= Just Let,
        testCase "In" $ tokenise' "in" @?= Just In,

        testCase "Fun" $ tokenise' "fun" @?= Just Fun,

        testCase "Begin" $ tokenise' "begin" @?= Just Begin,
        testCase "End" $ tokenise' "end" @?= Just End,

        testCase "While" $ tokenise' "while" @?= Just While,
        testCase "Do" $ tokenise' "do" @?= Just Do,

        testCase "Unit" $ tokenise' "()" @?= Just Unit,
        testProperty "Integer" $ forAll integers (\(i,s) -> tokenise' s === Just (Integer i)),
        testCase "Boolean True" $ tokenise' "true" @?= Just (Boolean True),
        testCase "Boolean False" $ tokenise' "false" @?= Just (Boolean False),

        testCase "Input" $ tokenise' "?" @?= Just Input,

        testProperty "Identifier" $ forAll identifiers (\i -> tokenise' i === Just (Identifier i)),

        testCase "Int Type" $ tokenise' "int" @?= Just IntType,
        testCase "Bool Type" $ tokenise' "bool" @?= Just BoolType,
        testCase "Unit Type" $ tokenise' "unit" @?= Just UnitType
    ]