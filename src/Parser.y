{
module Parser
(
    parse
) where

import Lexer
import qualified Lexer.Tokens as L

import qualified Parser.Types as T
import qualified Parser.Expressions as E

-- Need to fix shift/reduce conflicts
-- https://github.com/Timothy-G-Griffin/cc_cl_cam_ac_uk/blob/master/slang/parser.mly

}

%name slangParse
%tokentype { Token }
%error { parseError }
%monad { Alex }
%lexer { lexerWrapper } { (L.EOF, _) }

%token
    '('         { (L.LParen, _) }
    ')'         { (L.RParen, _) }
    ','         { (L.Comma, _) }
    ':'         { (L.Colon, _) }
    ';'         { (L.Semicolon, _) }

    '+'         { (L.Add, _) }
    '-'         { (L.Sub, _) }
    '*'         { (L.Mult, _) }
    '/'         { (L.Div, _) }

    '='         { (L.Equal, _) }
    '<'         { (L.Less, _) }
    '~'         { (L.Not, _) }
    '&&'        { (L.And, _) }
    '||'        { (L.Or, _) }

    '|'         { (L.Pipe, _) }
    '->'        { (L.Arrow, _) }

    ref         { (L.Ref, _) }
    ':='        { (L.Assign, _) }
    '!'         { (L.Deref, _) }

    inl         { (L.Inl, _) }
    inr         { (L.Inr, _) }
    case        { (L.Case, _) }
    of          { (L.Of, _) }

    fst         { (L.Fst, _) }
    snd         { (L.Snd, _) }

    if          { (L.If, _) }
    then        { (L.Then, _) }
    else        { (L.Else, _) }

    let         { (L.Let, _) }
    rec         { (L.Rec, _) }
    in          { (L.In, _) }

    fun         { (L.Fun, _) }

    begin       { (L.Begin, _) }
    end         { (L.End, _) }

    while       { (L.While, _) }
    do          { (L.Do, _) }

    unit        { (L.Unit, _) }
    integer     { (L.Integer $$, _) }
    boolean     { (L.Boolean $$, _) }

    identifier  { (L.Identifier $$, _) }

    inttype     { (L.IntType, _) }
    booltype    { (L.BoolType, _) }
    unittype    { (L.UnitType, _) }


-- Lowest precedence
%left '+' '-'
%left '*' '/' '&&' '||' '=' '->' '<'
%left ':='

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
        | Expr '<' Expr                                         { E.ArithBinaryOp E.OpLess $1 $3 }
        | Expr '=' Expr                                         { E.BinaryOp E.OpEqual $1 $3 }
        | Expr '&&' Expr                                        { E.BoolBinaryOp E.OpAnd $1 $3 }
        | Expr '||' Expr                                        { E.BoolBinaryOp E.OpOr $1 $3 }
        | Expr ':=' Expr                                        { E.BinaryOp E.OpAssign $1 $3 }
        | begin ExprList end                                    { E.Sequence $2 }
        | if Expr then Expr else Expr end                       { E.If $2 $4 $6 }
        | while Expr do Expr                                    { E.While $2 $4 }
        | fst Expr  %prec uminus                                { E.Fst $2 }
        | snd Expr  %prec uminus                                { E.Snd $2 }
        | inl Expr ':' Type                                     { E.Inl $2 $4 }
        | inr Expr ':' Type                                     { E.Inr $2 $4 }
        | fun '(' identifier ':' Type ')' '->' Expr end         { E.Fun $3 $5 $8 }
        | let identifier ':' Type '=' Expr in Expr end          { E.Let $2 $4 $6 $8 }

        | let identifier '(' identifier ':' Type ')'
                    ':' Type '=' Expr in Expr end               { E.LetFun $2 (E.Fun $4 $6 $11) $9 $13 }

        | let rec identifier '(' identifier ':' Type ')'
                    ':' Type '=' Expr in Expr end               { E.LetRecFun $3 (E.Fun $5 $7 $12) $10 $14 }

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
        | ref Type          { T.Ref $2 }
        | Type '->' Type    { T.Fun $1 $3 }
        | Type '*' Type     { T.Product $1 $3 }
        | Type '+' Type     { T.Union $1 $3 }
        | '(' Type ')'      { $2 }


{
parseError :: Token -> Alex a
parseError t = alexError $ "Parse Error: " ++ show t
    
lexerWrapper :: (Token -> Alex a) -> Alex a
lexerWrapper f = do
        token <- alexMonadScan
        f token
    
parse :: String -> Either String E.Expr
parse s = runAlex s slangParse
}