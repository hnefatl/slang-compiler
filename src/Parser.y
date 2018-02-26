{
module Parser
(
    parse,
    parser,
    parserT
) where

import Lexer
import qualified Lexer.Tokens as L

import qualified Parser.Types as T
import qualified Parser.Expressions as E

import Control.Monad.Except

import Common
}

%name slangParse
%tokentype { L.Token }
%error { parseError }
%monad { Parser }
%lexer { lexer } { L.EOF }

%token
    '('         { L.LParen }
    ')'         { L.RParen }
    ','         { L.Comma }
    ':'         { L.Colon }
    ';'         { L.Semicolon }

    '+'         { L.Add }
    '-'         { L.Sub }
    '*'         { L.Mult }
    '/'         { L.Div }

    '='         { L.Equal }
    '<'         { L.Less }
    '~'         { L.Not }
    '&&'        { L.And }
    '||'        { L.Or }

    '|'         { L.Pipe }
    '->'        { L.Arrow }

    ref         { L.Ref }
    ':='        { L.Assign }
    '!'         { L.Deref }

    inl         { L.Inl }
    inr         { L.Inr }
    case        { L.Case }
    of          { L.Of }

    fst         { L.Fst }
    snd         { L.Snd }

    if          { L.If }
    then        { L.Then }
    else        { L.Else }

    let         { L.Let }
    in          { L.In }

    fun         { L.Fun }

    begin       { L.Begin }
    end         { L.End }

    while       { L.While }
    do          { L.Do }

    unit        { L.Unit }
    integer     { L.Integer $$ }
    boolean     { L.Boolean $$ }

    identifier  { L.Identifier $$ }

    input       { L.Input }

    inttype     { L.IntType }
    booltype    { L.BoolType }
    unittype    { L.UnitType }


-- Lowest precedence
%left ':='
%left '+' '-'
%left '*' '/' '&&' '||' '=' '->' '<'

%nonassoc negate

%nonassoc then
%nonassoc else

%nonassoc uminus

%nonassoc unit integer boolean identifier '(' '~' '!' ref
-- Highest precedence

%%

Expr :: { E.Expr }
Expr    : SimpleExpr                                            { E.SimpleExpr $1 }
        | '-' Expr  %prec unit                                  { E.UnaryOp E.OpNeg $2 }
        | '~' Expr  %prec unit                                  { E.UnaryOp E.OpNot $2 }
        | Expr SimpleExpr                                       { E.Application $1 $2 }
            -- Unary negation has high precedence
        | Expr '+' Expr                                         { E.ArithBinaryOp E.OpAdd $1 $3 }
        | Expr '-' Expr                                         { E.ArithBinaryOp E.OpSub $1 $3 }
        | Expr '*' Expr                                         { E.ArithBinaryOp E.OpMul $1 $3 }
        | Expr '/' Expr                                         { E.ArithBinaryOp E.OpDiv $1 $3 }
        | Expr '=' Expr                                         { E.BinaryOp E.OpEqual $1 $3 }
        | Expr '<' Expr                                         { E.BinaryOp E.OpLess $1 $3 }
        | Expr ':=' Expr                                        { E.BinaryOp E.OpAssign $1 $3 }
        | Expr '&&' Expr                                        { E.BoolBinaryOp E.OpAnd $1 $3 }
        | Expr '||' Expr                                        { E.BoolBinaryOp E.OpOr $1 $3 }
        | begin ExprList end                                    { E.Sequence $2 }
        | if Expr then Expr else Expr end                       { E.If $2 $4 $6 }
        | while Expr do Expr end                                { E.While $2 $4 }
        | fst Expr  %prec uminus                                { E.Fst $2 }
        | snd Expr  %prec uminus                                { E.Snd $2 }
        | inl Type Expr                                         { E.Inl $3 $2 }
        | inr Type Expr                                         { E.Inr $3 $2 }
        | input                                                 { E.Input }
        | fun '(' identifier ':' Type ')' '->' Expr end         { E.Fun $3 $5 $8 }
        | let identifier ':' Type '=' Expr in Expr end          { E.Let $2 $4 $6 $8 }

        | let identifier '(' identifier ':' Type ')'
                    ':' Type '=' Expr in Expr end               { E.LetFun $2 (E.Fun $4 $6 $11) $9 $13 }

        | case Expr of
                inl '(' identifier ':' Type ')' '->' Expr
            '|' inr '(' identifier ':' Type ')' '->' Expr
          end                                                   { E.Case $2 (E.Fun $6 $8 $11) (E.Fun $15 $17 $20)}
        | case Expr of
                inr '(' identifier ':' Type ')' '->' Expr
            '|' inl '(' identifier ':' Type ')' '->' Expr
          end                                                   { E.Case $2 (E.Fun $15 $17 $20) (E.Fun $6 $8 $11) }


SimpleExpr :: { E.SimpleExpr }
SimpleExpr  : unit                          { E.Unit }
            | integer                       { E.Integer $1 }
            | boolean                       { E.Boolean $1 }
            | identifier                    { E.Identifier $1 }
            | '(' Expr ')'                  { E.Expr $2 }
            | '(' Expr ',' Expr ')'         { E.Pair $2 $4 }
            | '!' SimpleExpr                { E.Deref $2 }
            | ref SimpleExpr                { E.Ref $2 }

ExprList :: { [E.Expr] }
ExprList    : Expr                  { [$1] }
            | Expr ';' ExprList     { $1:$3 }

Type :: { T.Type }
Type    : inttype           { T.Integer }
        | booltype          { T.Boolean }
        | unittype          { T.Unit }
        | Type ref          { T.Ref $1 }
        | Type '->' Type    { T.Fun $1 $3 }
        | Type '*' Type     { T.Product $1 $3 }
        | Type '+' Type     { T.Union $1 $3 }
        | '(' Type ')'      { $2 }


{

parseError :: L.Token -> Parser a
parseError t = do
                Position a r c <- getPosition
                rawError ("Parser Error (row " ++ show r ++ ", col " ++ show c ++ ", abs " ++ show a ++ "): " ++ show t)

parse :: String -> Either Error E.Expr
parse s = runParser s slangParse

parser :: String -> Except Error E.Expr
parser = parserT

parserT :: Monad m => String -> ExceptT Error m E.Expr
parserT = ExceptT . return . parse

}