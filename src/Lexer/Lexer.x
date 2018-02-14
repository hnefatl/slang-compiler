{
module Lexer.Lexer
(
    Token(..),
    tokenise,
    main
) where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-
    $white+             ;  -- Skip whitespace
    \-?$digit+          { \s -> Integer (read s) }
    True                { \_ -> Boolean True }
    False               { \_ -> Boolean False }

{
data Token = Integer Integer
           | Boolean Bool
           deriving (Eq, Show)

tokenise :: String -> [Token]
tokenise = alexScanTokens

main :: IO ()
main = do
    c <- getContents
    print $ tokenise c
}