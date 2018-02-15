{
module Parser.Parser
(
    parse
) where

import Lexer.Lexer
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




%left '+' '-'
%left '*' '/' '&&' '||' '=' '->' '<'
%left ':='

%%

Expr    : SimpleExpr                                            { E.SimpleExpr $1 }
        | Expr SimpleExpr                                       { E.Apply $1 $2 }
        | '-' Expr                                              { E.UnaryOp E.OpNeg $2 }
        | Expr '+' Expr                                         { E.BinaryOp E.OpAdd $1 $3 }
        | Expr '-' Expr                                         { E.BinaryOp E.OpSub $1 $3 }
        | Expr '*' Expr                                         { E.BinaryOp E.OpMul $1 $3 }
        | Expr '/' Expr                                         { E.BinaryOp E.OpDiv $1 $3 }
        | Expr '<' Expr                                         { E.BinaryOp E.OpLess $1 $3 }
        | Expr '=' Expr                                         { E.BinaryOp E.OpEqual $1 $3 }
        | Expr '&&' Expr                                        { E.BinaryOp E.OpAnd $1 $3 }
        | Expr '||' Expr                                        { E.BinaryOp E.OpOr $1 $3 }
        | Expr ':=' Expr                                        { E.BinaryOp E.OpAssign $1 $3 }
        | begin ExprList end                                    { E.Sequence $2 }
        | if Expr then Expr else Expr end                       { E.If $2 $4 $6 }
        | while Expr do Expr                                    { E.While $2 $4 }
        | fst Expr                                              { E.Fst $2 }
        | snd Expr                                              { E.Snd $2 }
        | inl Expr                                              { E.Inl $2 }
        | inr Expr                                              { E.Inr $2 }
        | fun '(' identifier ':' Type ')' '->' Expr end         { E.Fun (E.Lambda $3 $5 $8) }
        | let identifier ':' Type '=' Expr in Expr end          { E.Let $2 $4 $6 $8 }

        | let identifier '(' identifier ':' Type ')'
                    ':' Type '=' Expr in Expr end               { E.LetFun $2 (E.Lambda $4 $6 $11) $9 $13 }

        | case Expr of
                inl '(' identifier ':' Type ')' '->' Expr
            '|' inr '(' identifier ':' Type ')' '->' Expr
          end                                                   { E.Case $2 (E.Lambda $6 $8 $11) (E.Lambda $15 $17 $20)}
        | case Expr of
                inr '(' identifier ':' Type ')' '->' Expr
            '|' inl '(' identifier ':' Type ')' '->' Expr
          end                                                   { E.Case $2 (E.Lambda $15 $17 $20) (E.Lambda $6 $8 $11) }


SimpleExpr  : unit                      { E.Unit }
            | integer                   { E.Integer $1 }
            | boolean                   { E.Boolean $1 }
            | identifier                { E.Identifier $1 }
            | '(' Expr ')'              { E.Expr $2 }
            | '(' Expr ',' Expr ')'     { E.Pair $2 $4 }
            | '~' SimpleExpr            { E.Not $2 }
            | '!' SimpleExpr            { E.Deref $2 }
            | ref SimpleExpr            { E.Ref $2 }


ExprList    : Expr                  { [$1] }
            | Expr ';' ExprList     { $1:$3 }


Type    : inttype           { T.Int }
        | booltype          { T.Bool }
        | unittype          { T.Unit }
        | ref Type          { T.Ref $2 }
        | Type '->' Type    { T.Fn $1 $3 }
        | Type '*' Type     { T.Product $1 $3 }
        | Type '+' Type     { T.Union $1 $3 }
        | '(' Type ')'      { $2 }


{
parseError :: Token -> Alex a
parseError ts = alexError $ "Parse Error: " ++ show ts
    
lexerWrapper :: (Token -> Alex a) -> Alex a
lexerWrapper f = do
        token <- alexMonadScan
        f token
    
parse :: String -> Either String E.Expr
parse s = runAlex s slangParse
}