{
-- If Alex includes too many modules, don't generate warnings
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Lexer
(
    reservedTokens,
    Alex(..),
    alexMonadScan,
    runAlex,
    alexEOF,
    alexError,
    Token,
    tokenise
) where

import Data.Char (toLower, toUpper)
import Data.Either (either)

import qualified Lexer.Tokens as T
}


%wrapper "monad"

$digit = 0-9
$alpha = [a-zA-Z]

@whitespace = $white+
@natural = $digit+
@boolean = (true) | (false)
@identifier = $alpha [$alpha $digit \']*

tokens :-
    @whitespace         ;  -- Skip whitespace
    
    "("                 { makeConstAlexToken T.LParen }
    ")"                 { makeConstAlexToken T.RParen }
    ","                 { makeConstAlexToken T.Comma }
    ";"                 { makeConstAlexToken T.Semicolon }
    ":"                 { makeConstAlexToken T.Colon }

    "+"                 { makeConstAlexToken T.Add }
    "-"                 { makeConstAlexToken T.Sub }
    "*"                 { makeConstAlexToken T.Mult }
    "/"                 { makeConstAlexToken T.Div }
    
    "="                 { makeConstAlexToken T.Equal }
    "<"                 { makeConstAlexToken T.Less }
    "~"                 { makeConstAlexToken T.Not }
    "&&"                { makeConstAlexToken T.And}
    "||"                { makeConstAlexToken T.Or}

    "|"                 { makeConstAlexToken T.Pipe }
    "->"                { makeConstAlexToken T.Arrow }

    "ref"               { makeConstAlexToken T.Ref }
    ":="                { makeConstAlexToken T.Assign }
    "!"                 { makeConstAlexToken T.Deref }

    "inl"               { makeConstAlexToken T.Inl }
    "inr"               { makeConstAlexToken T.Inr }
    "case"              { makeConstAlexToken T.Case }
    "of"                { makeConstAlexToken T.Of }

    "fst"               { makeConstAlexToken T.Fst }
    "snd"               { makeConstAlexToken T.Snd }

    "if"                { makeConstAlexToken T.If }
    "then"              { makeConstAlexToken T.Then }
    "else"              { makeConstAlexToken T.Else }

    "let"               { makeConstAlexToken T.Let }
    "in"                { makeConstAlexToken T.In }

    "fun"               { makeConstAlexToken T.Fun }

    "begin"             { makeConstAlexToken T.Begin }
    "end"               { makeConstAlexToken T.End }

    "while"             { makeConstAlexToken T.While }
    "do"                { makeConstAlexToken T.Do }

    "()"                { makeConstAlexToken T.Unit }
    @natural            { makeAlexToken (T.Integer . read) }
    @boolean            { makeAlexToken (T.Boolean . read . capitalise) }

    "?"                 { makeConstAlexToken T.Input }

    "int"               { makeConstAlexToken T.IntType }
    "bool"              { makeConstAlexToken T.BoolType }
    "unit"              { makeConstAlexToken T.UnitType }

    @identifier         { makeAlexToken T.Identifier }
    
{
reservedTokens :: [String]
reservedTokens = ["ref", "inl", "inr", "case", "of", "fst", "snd", "if", "then", "else", "let", "in", "fun", "begin", "end", "while", "do", "int", "bool", "unit"]

-- Construct a token in the Alex monad given a function to convert an input to a token
makeAlexToken :: (String -> T.TokenClass) -> AlexInput -> Int -> Alex Token
makeAlexToken f (p, _, _, s) len = return (f $ take len s, p)

-- Construct a token in the Alex monad given a constant token
makeConstAlexToken :: T.TokenClass -> AlexInput -> Int -> Alex Token
makeConstAlexToken t = makeAlexToken (const t)

type Token = (T.TokenClass, AlexPosn)

alexEOF :: Alex Token
alexEOF = do
    (pos, _, _, _) <- alexGetInput
    return (T.EOF, pos)

capitalise :: String -> String
capitalise "" = ""
capitalise (c:cl) = [toUpper c] ++ map toLower cl

tokenise :: String -> Either String Token
tokenise s = runAlex s alexMonadScan
}