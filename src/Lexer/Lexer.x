{
module Lexer.Lexer
(
    Token(..),
    tokenise,
    capitalise,
    main
) where

import Data.Char (toLower, toUpper)
}


%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

@whitespace = $white+
@integer = "-"? $digit+
@boolean = (true) | (false)

tokens :-
    @whitespace         ;  -- Skip whitespace
    
    "("                 { \_ -> LParen }
    ")"                 { \_ -> RParen }
    ","                 { \_ -> Comma }
    ";"                 { \_ -> Semicolon }
    ":"                 { \_ -> Colon }

    "+"                 { \_ -> Add }
    "-"                 { \_ -> Sub }
    "*"                 { \_ -> Mult }
    "/"                 { \_ -> Div }
    
    "="                 { \_ -> Equal }
    "<"                 { \_ -> Less }
    "~"                 { \_ -> Not }
    "&&"                { \_ -> And}
    "||"                { \_ -> Or}

    "|"                 { \_ -> Pipe }
    "->"                { \_ -> Arrow }

    "ref"               { \_ -> Ref }
    ":="                { \_ -> Assign }
    "!"                 { \_ -> Deref }

    "inl"               { \_ -> Inl }
    "inr"               { \_ -> Inr }
    "case"              { \_ -> Case }
    "of"                { \_ -> Of }

    "fst"               { \_ -> Fst }
    "snd"               { \_ -> Snd }

    "if"                { \_ -> If }
    "then"              { \_ -> Then }
    "else"              { \_ -> Else }

    "let"               { \_ -> Let }
    "in"                { \_ -> In }

    "fun"               { \_ -> Fun }

    "begin"             { \_ -> Begin }
    "end"               { \_ -> End }

    "while"             { \_ -> While }
    "do"                { \_ -> Do }

    "()"                { \_ -> Unit }
    @integer            { \s -> Integer (read s) }
    @boolean            { \b -> Boolean (read $ capitalise b) }

    "?"                 { \_ -> Input }


{
data Token  = LParen
            | RParen
            | Comma
            | Colon
            | Semicolon

            | Add
            | Sub
            | Mult
            | Div

            | Equal
            | Less
            | Not
            | And
            | Or
            
            | Pipe
            | Arrow

            | Ref
            | Assign
            | Deref

            | Inl
            | Inr
            | Case
            | Of

            | Fst
            | Snd

            | If
            | Then
            | Else

            | Let
            | In

            | Fun

            | Begin
            | End

            | While
            | Do

            | Unit
            | Integer Integer
            | Boolean Bool

            | Input
            deriving (Eq, Show)

capitalise :: String -> String
capitalise "" = ""
capitalise (c:cl) = [toUpper c] ++ map toLower cl

tokenise :: String -> [Token]
tokenise = alexScanTokens

main :: IO ()
main = do
    c <- getContents
    print $ tokenise c
}