{
module Parser.Parser
(
    parse
) where

import Lexer.Lexer
import qualified Lexer.Tokens as L

import Parser.Types
import Parser.Expressions
}

%name slangParse
%tokentype { Token }
%error { parseError }
%monad { Alex }
%lexer { lexerWrapper } { (L.EOF, _) }

%token
    '('         { (L.LParen, _) }
    ')'         { (L.RParen, _) }

    '+'         { (L.Add, _) }
    '-'         { (L.Sub, _) }
    '*'         { (L.Mult, _) }
    '/'         { (L.Div, _) }

    not         { (L.Not, _) }

    unit        { (L.Unit, _) }
    integer     { (L.Integer $$, _) }

%left '+' '-'
%left '*' '/'

%%

Expr    : Expr '+' Expr             { BinaryOp Add $1 $3 }
        | Expr '-' Expr             { BinaryOp Sub $1 $3 }
        | Expr '*' Expr             { BinaryOp Mul $1 $3 }
        | Expr '/' Expr             { BinaryOp Div $1 $3 }
        | SimpleExpr                { SimpleExpr $1 }

SimpleExpr  : unit                  { Unit }
            | integer               { Integer $1 }

UOp : not       {  }


{
parseError :: Token -> Alex a
parseError ts = alexError $ "Parse Error: " ++ show ts
    
lexerWrapper :: (Token -> Alex a) -> Alex a
lexerWrapper f = do
        token <- alexMonadScan
        f token
    
parse :: String -> Either String Expr
parse s = runAlex s slangParse
}