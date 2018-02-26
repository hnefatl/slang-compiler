{
-- If Alex includes too many modules, don't generate warnings
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lexer
(
    reservedTokens,
    Parser,
    runParser,
    parserEOF,
    rawError,
    lexError,
    lexer,
    tokenise
) where

import Data.Char (toLower, toUpper)
import Data.Either (either)

import Control.Monad.Except
import Control.Monad.State

import qualified Lexer.Tokens as T
import Common
}

%wrapper "monad"

$digit = 0-9
$alpha = [a-zA-Z]

@whitespace = $white+
@natural = $digit+
@boolean = (true) | (false)
@identifier = $alpha [$alpha $digit \']*
@commentskippable = @whitespace | .*

tokens :-
    @whitespace         ;  -- Skip whitespace
    
    "("                 { makeConstToken T.LParen }
    ")"                 { makeConstToken T.RParen }
    ","                 { makeConstToken T.Comma }
    ";"                 { makeConstToken T.Semicolon }
    ":"                 { makeConstToken T.Colon }

    "+"                 { makeConstToken T.Add }
    "-"                 { makeConstToken T.Sub }
    "*"                 { makeConstToken T.Mult }
    "/"                 { makeConstToken T.Div }
    
    "="                 { makeConstToken T.Equal }
    "<"                 { makeConstToken T.Less }
    "~"                 { makeConstToken T.Not }
    "&&"                { makeConstToken T.And}
    "||"                { makeConstToken T.Or}

    "|"                 { makeConstToken T.Pipe }
    "->"                { makeConstToken T.Arrow }

    "ref"               { makeConstToken T.Ref }
    ":="                { makeConstToken T.Assign }
    "!"                 { makeConstToken T.Deref }

    "inl"               { makeConstToken T.Inl }
    "inr"               { makeConstToken T.Inr }
    "case"              { makeConstToken T.Case }
    "of"                { makeConstToken T.Of }

    "fst"               { makeConstToken T.Fst }
    "snd"               { makeConstToken T.Snd }

    "if"                { makeConstToken T.If }
    "then"              { makeConstToken T.Then }
    "else"              { makeConstToken T.Else }

    "let"               { makeConstToken T.Let }
    "in"                { makeConstToken T.In }

    "fun"               { makeConstToken T.Fun }

    "begin"             { makeConstToken T.Begin }
    "end"               { makeConstToken T.End }

    "while"             { makeConstToken T.While }
    "do"                { makeConstToken T.Do }

    "()"                { makeConstToken T.Unit }
    @natural            { makeToken (T.Integer . read) }
    @boolean            { makeToken (T.Boolean . read . capitalise) }

    "?"                 { makeConstToken T.Input }

    "int"               { makeConstToken T.IntType }
    "bool"              { makeConstToken T.BoolType }
    "unit"              { makeConstToken T.UnitType }

    "(*" @commentskippable* "*)"        ;

    @identifier         { makeToken T.Identifier }
    
{
reservedTokens :: [String]
reservedTokens = ["ref", "inl", "inr", "case", "of", "fst", "snd", "if", "then", "else", "let", "in", "fun", "begin", "end", "while", "do", "int", "bool", "unit"]

-- Defined our own parser types and functions that just alias through to the underlying "Alex" type
-- that's fixed and used by the lexer
type Parser a = Alex a
type ParserState = AlexState
type ParserInput = AlexInput

data Position = Position
    {
        absCharPos  :: Int, -- Absolute character position in the file
        row         :: Int, -- Line number
        col         :: Int  -- Column number
    }

getPosition :: ParserState -> Position
getPosition AlexState { alex_pos = AlexPn a l c } = Position a l c

runParser :: String -> Parser a -> Either Error a
runParser = runAlex

rawError :: Error -> Parser a
rawError = alexError

lexError :: Error -> Parser a
lexError err = rawError $ "Lex Error: " ++ err

lexer :: (T.Token -> Parser a) -> Parser a
lexer f = alexMonadScan >>= f

parserEOF :: Parser T.Token
parserEOF = return T.EOF

-- Another required defn for Alex
alexEOF :: Parser T.Token
alexEOF = parserEOF


-- Construct a token in the Parser monad given a function to convert an input to a token
makeToken :: (String -> T.Token) -> ParserInput -> Int -> Parser T.Token
makeToken f (_, _, _, s) len = return $ f $ take len s

-- Construct a token in the Alex monad given a constant token
makeConstToken :: T.Token -> ParserInput -> Int -> Parser T.Token
makeConstToken t = makeToken (const t)


capitalise :: String -> String
capitalise "" = ""
capitalise (c:cl) = [toUpper c] ++ map toLower cl

tokenise :: String -> Either Error T.Token
tokenise s = runParser s alexMonadScan
}