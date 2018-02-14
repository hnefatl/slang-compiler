module Lexer.Tokens
(
    Line,
    Column,
    Position,
    TokenPosition,
    token,
    pos,
    Token(..)
) where

type Line = Int
type Column = Int
type Position = (Line, Column)

type TokenPosition = (Token, Position)

token :: TokenPosition -> Token
token = fst

pos :: TokenPosition -> Position
pos = snd

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