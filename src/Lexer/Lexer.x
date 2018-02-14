{
module Lexer.Lexer
(
    tokenise,
    tokenise',
    makeTokenPosition,
    makeConstTokenPosition
) where

import Data.Char (toLower, toUpper)

import Lexer.Tokens
}


%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]

@whitespace = $white+
@integer = "-"? $digit+
@boolean = (true) | (false)

tokens :-
    @whitespace         ;  -- Skip whitespace
    
    "("                 { makeConstTokenPosition LParen }
    ")"                 { makeConstTokenPosition RParen }
    ","                 { makeConstTokenPosition Comma }
    ";"                 { makeConstTokenPosition Semicolon }
    ":"                 { makeConstTokenPosition Colon }

    "+"                 { makeConstTokenPosition Add }
    "-"                 { makeConstTokenPosition Sub }
    "*"                 { makeConstTokenPosition Mult }
    "/"                 { makeConstTokenPosition Div }
    
    "="                 { makeConstTokenPosition Equal }
    "<"                 { makeConstTokenPosition Less }
    "~"                 { makeConstTokenPosition Not }
    "&&"                { makeConstTokenPosition And}
    "||"                { makeConstTokenPosition Or}

    "|"                 { makeConstTokenPosition Pipe }
    "->"                { makeConstTokenPosition Arrow }

    "ref"               { makeConstTokenPosition Ref }
    ":="                { makeConstTokenPosition Assign }
    "!"                 { makeConstTokenPosition Deref }

    "inl"               { makeConstTokenPosition Inl }
    "inr"               { makeConstTokenPosition Inr }
    "case"              { makeConstTokenPosition Case }
    "of"                { makeConstTokenPosition Of }

    "fst"               { makeConstTokenPosition Fst }
    "snd"               { makeConstTokenPosition Snd }

    "if"                { makeConstTokenPosition If }
    "then"              { makeConstTokenPosition Then }
    "else"              { makeConstTokenPosition Else }

    "let"               { makeConstTokenPosition Let }
    "in"                { makeConstTokenPosition In }

    "fun"               { makeConstTokenPosition Fun }

    "begin"             { makeConstTokenPosition Begin }
    "end"               { makeConstTokenPosition End }

    "while"             { makeConstTokenPosition While }
    "do"                { makeConstTokenPosition Do }

    "()"                { makeConstTokenPosition Unit }
    @integer            { makeTokenPosition (Integer . read) }
    @boolean            { makeTokenPosition (Boolean . read . capitalise) }

    "?"                 { makeConstTokenPosition Input }


{

-- Converts a AlexPosn (token position) and a string into a Position (our representation of a position) and a Token
makeTokenPosition :: (String -> Token) -> (AlexPosn -> String -> TokenPosition)
makeTokenPosition f (AlexPn _ l c) s = (f s, (l, c))

-- Creates a TokenPosition using a constant token
makeConstTokenPosition :: Token -> (AlexPosn -> String -> TokenPosition)
makeConstTokenPosition t = makeTokenPosition (const t)

capitalise :: String -> String
capitalise "" = ""
capitalise (c:cl) = [toUpper c] ++ map toLower cl

tokenise :: String -> [TokenPosition]
tokenise = alexScanTokens

tokenise' :: String -> [Token]
tokenise' = map token . tokenise

main :: IO ()
main = do
    c <- getContents
    print $ tokenise c
}