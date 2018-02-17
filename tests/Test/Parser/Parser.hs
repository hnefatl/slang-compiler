module Test.Parser.Parser where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Parser.Parser
import Parser.Expressions

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