module Test.Parser.Parser where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Parser.Parser
import Parser.Expressions
import qualified Parser.Types as T

import Test.Util

parserTests :: TestTree
parserTests = testGroup "Parser"
    [
        simpleExprTests,
        exprTests
    ]

simpleExprTests :: TestTree
simpleExprTests = testGroup "SimpleExpr"
    [
        testCase "Unit" $ parse "()" @?= (Right . SimpleExpr) Unit,
        testProperty "Integer" $ forAll integers $ \(i,s) -> parse s === (Right . SimpleExpr . Integer) i,
        testProperty "Boolean" $ forAll booleans $ \(b,s) -> parse s === (Right . SimpleExpr . Boolean) b,
        testProperty "Identifer" $ forAll identifiers $ \i -> parse i == (Right . SimpleExpr . Identifier) i

    ]

exprTests :: TestTree
exprTests = testGroup "Expr"
    [
        testGroup "UnaryOp"
        [
            testProperty "OpNot" $
                forAll booleans $ \(b,s) ->
                    parse ("~" ++ s)
                    ===
                    (Right . (UnaryOp OpNot) . SimpleExpr . Boolean) b,
            testProperty "OpNeg" $
                forAll integers $ \(i,s) ->
                    parse ("-" ++ s)
                    ===
                    (Right . (UnaryOp OpNeg) . SimpleExpr . Integer) i
        ],
        testGroup "ArithBinaryOp"
        [
            testProperty "OpAdd" (arithBOpHelper OpAdd "+"),
            testProperty "OpSub" (arithBOpHelper OpSub "-"),
            testProperty "OpMul" (arithBOpHelper OpMul "*"),
            testProperty "OpDiv" (arithBOpHelper OpDiv "/"),
            testProperty "OpLess" (arithBOpHelper OpLess "<")
        ],
        testGroup "BoolBinaryOp"
        [
            testProperty "OpAnd" (boolBOpHelper OpAnd "&&"),
            testProperty "OpOr" (boolBOpHelper OpOr "||")
        ],
        testGroup "BinaryOp"
        [
            testCase "OpEqual" $ parse "17 = 18" @?= (Right $ BinaryOp OpEqual (SimpleExpr $ Integer 17) (SimpleExpr $ Integer 18)),
            testCase "OpEqual" $ parse "true = false" @?= (Right $ BinaryOp OpEqual (SimpleExpr $ Boolean True) (SimpleExpr $ Boolean False)),
            testCase "OpEqual" $ parse "() = ()" @?= (Right $ BinaryOp OpEqual (SimpleExpr Unit) (SimpleExpr Unit))
        ],
        testGroup "Sequence"
        [
            testCase "Int Sequence" $ parse "begin 17;16;15 end" @?= (Right $ Sequence [SimpleExpr (Integer 17), SimpleExpr (Integer 16), SimpleExpr (Integer 15)]),
            testCase "Bool Sequence" $ parse "begin true;false end" @?= (Right $ Sequence [SimpleExpr (Boolean True), SimpleExpr (Boolean False)])
        ],
        testGroup "If"
        [
            testCase "If Int" $ parse "if true then 1 else 0 end" @?= (Right $ If (SimpleExpr $ Boolean True) (SimpleExpr $ Integer 1) (SimpleExpr $ Integer 0)),
            testCase "If Bool" $ parse "if true then false else true end" @?= (Right $ If (SimpleExpr $ Boolean True) (SimpleExpr $ Boolean False) (SimpleExpr $ Boolean True))
        ],
        testGroup "Inl"
        [
            testCase "Inl Int+Bool" $ parse "inl 5 : int + bool" @?= (Right $ Inl (SimpleExpr $ Integer 5) (T.Union T.Integer T.Boolean))
        ]
    ]

-- Simple helper that constructs a property testing that an expression
-- like 10 + 5 is parsed correctly (OpAdd and "+" are the parameters, in that example)
arithBOpHelper :: ArithBOp -> String -> Property
arithBOpHelper op opString = 
    forAll (pairsOf integers) $ \((i1,s1), (i2,s2)) ->
        parse (s1 ++ opString ++ s2)
        ===
        Right ((ArithBinaryOp op) (SimpleExpr $ Integer i1) (SimpleExpr $ Integer i2))

boolBOpHelper :: BoolBOp -> String -> Property
boolBOpHelper op opString =
    forAll (pairsOf booleans) $ \((b1,s1), (b2,s2)) ->
        parse (s1 ++ opString ++ s2)
        ===
        Right ((BoolBinaryOp op) (SimpleExpr $ Boolean b1) (SimpleExpr $ Boolean b2))