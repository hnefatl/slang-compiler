{
module Lexer.Lexer
(
    Alex(..),
    alexMonadScan,
    runAlex,
    alexEOF,
    alexError,
    Token,
    tokenise,
    tokenise'
) where

import Data.Char (toLower, toUpper)
import Data.Either (either)

import Lexer.Tokens
}


%wrapper "monad"

$digit = 0-9
$alpha = [a-zA-Z]

@whitespace = $white+
@integer = "-"? $digit+
@boolean = (true) | (false)

tokens :-
    @whitespace         ;  -- Skip whitespace
    
    "("                 { makeConstAlexToken LParen }
    ")"                 { makeConstAlexToken RParen }
    ","                 { makeConstAlexToken Comma }
    ";"                 { makeConstAlexToken Semicolon }
    ":"                 { makeConstAlexToken Colon }

    "+"                 { makeConstAlexToken Add }
    "-"                 { makeConstAlexToken Sub }
    "*"                 { makeConstAlexToken Mult }
    "/"                 { makeConstAlexToken Div }
    
    "="                 { makeConstAlexToken Equal }
    "<"                 { makeConstAlexToken Less }
    "~"                 { makeConstAlexToken Not }
    "&&"                { makeConstAlexToken And}
    "||"                { makeConstAlexToken Or}

    "|"                 { makeConstAlexToken Pipe }
    "->"                { makeConstAlexToken Arrow }

    "ref"               { makeConstAlexToken Ref }
    ":="                { makeConstAlexToken Assign }
    "!"                 { makeConstAlexToken Deref }

    "inl"               { makeConstAlexToken Inl }
    "inr"               { makeConstAlexToken Inr }
    "case"              { makeConstAlexToken Case }
    "of"                { makeConstAlexToken Of }

    "fst"               { makeConstAlexToken Fst }
    "snd"               { makeConstAlexToken Snd }

    "if"                { makeConstAlexToken If }
    "then"              { makeConstAlexToken Then }
    "else"              { makeConstAlexToken Else }

    "let"               { makeConstAlexToken Let }
    "in"                { makeConstAlexToken In }

    "fun"               { makeConstAlexToken Fun }

    "begin"             { makeConstAlexToken Begin }
    "end"               { makeConstAlexToken End }

    "while"             { makeConstAlexToken While }
    "do"                { makeConstAlexToken Do }

    "()"                { makeConstAlexToken Unit }
    @integer            { makeAlexToken (Integer . read) }
    @boolean            { makeAlexToken (Boolean . read . capitalise) }

    "?"                 { makeConstAlexToken Input }


{

-- Construct a token in the Alex monad given a function to convert an input to a token
makeAlexToken :: (String -> TokenClass) -> AlexInput -> Int -> Alex Token
makeAlexToken f (p, _, _, s) len = return (f $ take len s, p)

-- Construct a token in the Alex monad given a constant token
makeConstAlexToken :: TokenClass -> AlexInput -> Int -> Alex Token
makeConstAlexToken t = makeAlexToken (const t)

type Token = (TokenClass, AlexPosn)

alexEOF :: Alex Token
alexEOF = do
    (pos, _, _, _) <- alexGetInput
    return (EOF, pos)

capitalise :: String -> String
capitalise "" = ""
capitalise (c:cl) = [toUpper c] ++ map toLower cl

tokenise :: String -> Either String Token
tokenise s = runAlex s alexMonadScan

tokenise' :: String -> Maybe TokenClass
tokenise' = either (const Nothing) (Just . fst) . tokenise
}