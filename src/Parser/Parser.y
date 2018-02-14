{
module Parser.Parser
(
    parse,
    parseString
) where

import Lexer.Lexer
import qualified Lexer.Tokens as L

import Parser.Types
import Parser.Expressions
}

%name parseInternal
%tokentype { L.TokenPosition }
%error { parseError }

%token
    '('         { (L.LParen, _) }
    ')'         { (L.RParen, _) }

    '+'         { (L.Add, _) }
    '-'         { (L.Sub, _) }
    '*'         { (L.Mult, _) }
    '/'         { (L.Div, _) }

%left '+' '-'
%left '*' '/'

%%

ExprPos : ExprPos '+' ExprPos        { (BinaryOp Add (expr $1) (expr $3), pos $1) }
        | ExprPos '-' ExprPos        { (BinaryOp Sub (expr $1) (expr $3), pos $1) }
        | ExprPos '*' ExprPos        { (BinaryOp Mul (expr $1) (expr $3), pos $1) }
        | ExprPos '/' ExprPos        { (BinaryOp Div (expr $1) (expr $3), pos $1) }


{
parseError :: [L.TokenPosition] -> a
parseError ts = error $ "Parse Error: " ++ show ts
    
parse :: [L.TokenPosition] -> ExprPos
parse = parseInternal

parseString :: String -> ExprPos
parseString = parse . tokenise

main :: IO ()
main = do
    c <- getContents
    print $ parse $ tokenise c
}