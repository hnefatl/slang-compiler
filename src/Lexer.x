{
-- If Alex includes too many modules, don't generate warnings
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lexer
(
    reservedTokens,
    Parser,
    getPosition,
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

-- Construct a token in the Parser monad (defined below) given a function to convert an input to a token
makeToken :: (String -> T.Token) -> ParserInput -> Int -> Parser T.Token
makeToken f (_, _, _, s) len = return $ f $ take len s

-- Construct a token in the Alex monad given a constant token
makeConstToken :: T.Token -> ParserInput -> Int -> Parser T.Token
makeConstToken t = makeToken (const t)


reservedTokens :: [String]
reservedTokens = ["ref", "inl", "inr", "case", "of", "fst", "snd", "if", "then", "else", "let", "in", "fun", "begin", "end", "while", "do", "int", "bool", "unit"]

-- Helper function for the Alex stuff that Alex itself doesn't expose
-- Don't think this is necessary, we can get position from alexGetInput
--getAlexState :: Alex AlexState
--getAlexState = Alex $ \s -> Right (s, s)

-- Defined our own parser types and functions that just alias through to the underlying "Alex" type
-- that's fixed and used by the lexer.
-- From here on we use Parser instead of Alex wherever possible (and outside the module we only see Parser)
type Parser a = Alex a
type ParserState = AlexState
type ParserInput = AlexInput

getPosition :: Parser Position
getPosition = do
                (AlexPn a l c, _, _, _) <- alexGetInput
                return (Position a l c)

getToken :: Parser String
getToken = do
            (_, _, _, input) <- alexGetInput
            return input

runParser :: String -> Parser a -> Either FrontEndError a
runParser = runAlex

rawError :: FrontEndError -> Parser a
rawError = alexError

lexError :: FrontEndError -> Parser a
lexError err = do
            Position a r c <- getPosition
            input <- getToken
            rawError ("Lex Error on " ++ input ++ " (row " ++ show r ++ ", col " ++ show c ++ ", abs " ++ show a ++ "): " ++ err)

lexer :: (T.Token -> Parser a) -> Parser a
lexer f = alexMonadScan >>= f

parserEOF :: Parser T.Token
parserEOF = return T.EOF

-- Another required defn for Alex
alexEOF :: Parser T.Token
alexEOF = parserEOF



capitalise :: String -> String
capitalise "" = ""
capitalise (c:cl) = [toUpper c] ++ map toLower cl

tokenise :: String -> Either FrontEndError T.Token
tokenise s = runParser s alexMonadScan
}