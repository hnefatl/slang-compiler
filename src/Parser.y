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

Expr :: { E.Expr Position }
Expr    : SimpleExpr                                            { E.SimpleExpr $1 }
        | '-' Expr  %prec unit                                  {% storePosition $ \p -> E.UnaryOp p E.OpNeg $2 }
        | '~' Expr  %prec unit                                  {% storePosition $ \p -> E.UnaryOp p E.OpNot $2 }
        | Expr SimpleExpr                                       {% storePosition $ \p -> E.Application p $1 $2 }
            -- Unary negation has high precedence
        | Expr '+' Expr                                         {% storePosition $ \p -> E.ArithBinaryOp p E.OpAdd $1 $3 }
        | Expr '-' Expr                                         {% storePosition $ \p -> E.ArithBinaryOp p E.OpSub $1 $3 }
        | Expr '*' Expr                                         {% storePosition $ \p -> E.ArithBinaryOp p E.OpMul $1 $3 }
        | Expr '/' Expr                                         {% storePosition $ \p -> E.ArithBinaryOp p E.OpDiv $1 $3 }
        | Expr '=' Expr                                         {% storePosition $ \p -> E.BinaryOp p E.OpEqual $1 $3 }
        | Expr '<' Expr                                         {% storePosition $ \p -> E.BinaryOp p E.OpLess $1 $3 }
        | Expr ':=' Expr                                        {% storePosition $ \p -> E.BinaryOp p E.OpAssign $1 $3 }
        | Expr '&&' Expr                                        {% storePosition $ \p -> E.BoolBinaryOp p E.OpAnd $1 $3 }
        | Expr '||' Expr                                        {% storePosition $ \p -> E.BoolBinaryOp p E.OpOr $1 $3 }
        | begin ExprList end                                    {% storePosition $ \p -> E.Sequence p $2 }
        | if Expr then Expr else Expr end                       {% storePosition $ \p -> E.If p $2 $4 $6 }
        | while Expr do Expr end                                {% storePosition $ \p -> E.While p $2 $4 }
        | fst Expr  %prec uminus                                {% storePosition $ \p -> E.Fst p $2 }
        | snd Expr  %prec uminus                                {% storePosition $ \p -> E.Snd p $2 }
        | inl Type Expr                                         {% storePosition $ \p -> E.Inl p $3 $2 }
        | inr Type Expr                                         {% storePosition $ \p -> E.Inr p $3 $2 }
        | input                                                 {% storePosition $ \p -> E.Input p }
        | fun '(' identifier ':' Type ')' '->' Expr end         {% storePosition $ \p -> E.Fun p $3 $5 $8 }
        | let identifier ':' Type '=' Expr in Expr end          {% storePosition $ \p -> E.Let p $2 $4 $6 $8 }

        | let identifier '(' identifier ':' Type ')'
                    ':' Type '=' Expr in Expr end               {% storePosition $ \p -> E.LetFun p $2 (E.Fun p $4 $6 $11) $9 $13 }

        -- Two case expressions: inl then inr, or inr then inl
        | case Expr of
                inl '(' identifier ':' Type ')' '->' Expr
            '|' inr '(' identifier ':' Type ')' '->' Expr
          end                                                   {% storePosition $ \p -> E.Case p $2 (E.Fun p $6 $8 $11) (E.Fun p $15 $17 $20) }
        | case Expr of
                inr '(' identifier ':' Type ')' '->' Expr
            '|' inl '(' identifier ':' Type ')' '->' Expr
          end                                                   {% storePosition $ \p -> E.Case p $2 (E.Fun p $15 $17 $20) (E.Fun p $6 $8 $11) }


SimpleExpr :: { E.SimpleExpr Position }
SimpleExpr  : '(' Expr ')'                  { E.Expr $2 }
            | unit                          {% storePosition $ \p -> E.Unit p }
            | integer                       {% storePosition $ \p -> E.Integer p $1 }
            | boolean                       {% storePosition $ \p -> E.Boolean p $1 }
            | identifier                    {% storePosition $ \p -> E.Identifier p $1 }
            | '(' Expr ',' Expr ')'         {% storePosition $ \p -> E.Pair p $2 $4 }
            | '!' SimpleExpr                {% storePosition $ \p -> E.Deref p $2 }
            | ref SimpleExpr                {% storePosition $ \p -> E.Ref p $2 }

ExprList :: { [E.Expr Position] }
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

storePosition :: (Position -> t Position) -> Parser (t Position)
storePosition e = getPosition >>= \p -> return $ e p

parseError :: L.Token -> Parser a
parseError t = do
                Position a r c <- getPosition
                rawError ("Parser Error (row " ++ show r ++ ", col " ++ show c ++ ", abs " ++ show a ++ "): " ++ show t)

parse :: String -> Either Error (E.Expr Position)
parse s = runParser s slangParse

parser :: String -> Except Error (E.Expr Position)
parser = parserT

parserT :: Monad m => String -> ExceptT Error m (E.Expr Position)
parserT = ExceptT . return . parse

}