module Lexer.Tokens
(
    TokenClass(..)
) where

data TokenClass = LParen
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

                | EOF
                deriving (Eq, Show)