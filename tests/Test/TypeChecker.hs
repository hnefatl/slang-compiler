module Test.TypeChecker where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Test.Util

import TypeChecker
import Parser
import Parser.Types

typecheck' :: String -> Either Error Type
typecheck' s = parse s >>= typecheck

typecheckerTests :: TestTree
typecheckerTests = testGroup "TypeChecker"
    [
        simpleExprTests,
        exprTests
    ]

simpleExprTests :: TestTree
simpleExprTests = testGroup "SimpleExpr"
    [
        testCase "Unit" $ typecheck' "()" @?= Right Unit,
        testProperty "Integer" $ forAll integers $ \(_,s) -> typecheck' s === (Right Integer),
        testProperty "Boolean" $ forAll booleans $ \(_,s) -> typecheck' s === (Right Boolean)
    ]

exprTests :: TestTree
exprTests = testGroup "Expr"
    [
        testGroup "UnaryOp"
        [
            testProperty "OpNot" $ forAll booleans $ \(_,s) -> typecheck' ("~" ++ s) === Right Boolean,
            testProperty "OpNeg" $ forAll integers $ \(_,s) -> typecheck' ("-" ++ s) === Right Integer
        ],
        testGroup "ArithBinaryOp"
        [
            testProperty "OpAdd" (arithBOpHelper "+"),
            testProperty "OpSub" (arithBOpHelper "-"),
            testProperty "OpMul" (arithBOpHelper "*"),
            testProperty "OpDiv" (arithBOpHelper "/"),
            testProperty "OpLess" (arithBOpHelper "<")
        ],
        testGroup "BoolBinaryOp"
        [
            testProperty "OpAnd" (boolBOpHelper "&&"),
            testProperty "OpOr" (boolBOpHelper "||")
        ],
        testGroup "BinaryOp"
        [
            testCase "OpEqual" $ typecheck' "17 = 18" @?= Right Boolean,
            testCase "OpEqual" $ typecheck' "true = false" @?= Right Boolean,
            testCase "OpEqual" $ typecheck' "() = ()" @?= Right Boolean
        ],
        testGroup "Sequence"
        [
            testCase "Int Sequence" $ typecheck' "begin 17;16;15 end" @?= Right Integer,
            testCase "Bool Sequence" $ typecheck' "begin true;false end" @?= Right Boolean
        ],
        testGroup "If"
        [
            testCase "If Int" $ typecheck' "if true then 1 else 0 end" @?= Right Integer,
            testCase "If Bool" $ typecheck' "if true then false else true end" @?= Right Boolean
        ],
        testGroup "Inl"
        [
            testCase "Inl Int+Bool" $ typecheck' "inl 5 : int + bool" @?= Right (Union Integer Boolean),
            testCase "Inl (Int+Bool)+Unit" $ typecheck' "inl (inl 5 : int + bool) : (int + bool) + unit" @?= Right (Union (Union Integer Boolean) Unit)
        ],
        testGroup "Inr"
        [
            testCase "Inr Int+Bool" $ typecheck' "inr true : int + bool" @?= Right (Union Integer Boolean),
            testCase "Inr (Int+Bool)+Unit" $ typecheck' "inr () : (int + bool) + unit" @?= Right (Union (Union Integer Boolean) Unit)
        ],
        testGroup "Case"
        [
            testCase "Case Inl 5" $ typecheck' "case inl 5 : int + bool of inl (x : int) -> true | inr (x : bool) -> x end" @?= Right Boolean
        ],
        testGroup "Fst"
        [
            testCase "Fst (1, 2)" $ typecheck' "fst (1, 2)" @?= Right Integer,
            testCase "Fst (true, (1,2))" $ typecheck' "fst (true, (1,2))" @?= Right Boolean
        ],
        testGroup "Snd"
        [
            testCase "Snd (1, 2)" $ typecheck' "snd (1, 2)" @?= Right Integer,
            testCase "Snd (true, (1,2))" $ typecheck' "snd (true, (1,2))" @?= Right (Product Integer Integer)
        ],
        testGroup "While"
        [
            testCase "While true do 5" $ typecheck' "while true do 5" @?= Right Integer,
            testCase "While Fst (true, 1) do 5" $ typecheck' "while fst (true, 1) do 5" @?= Right Integer,
            testCase "Fails: While Snd (true, 1) do 5" $ typecheck' "while snd (true, 1) do 5" ~?= Right Integer
        ],
        testGroup "Let"
        [
        ]
    ]

-- Simple helper that constructs a property testing that an expression
-- like 10 + 5 is parsed correctly (OpAdd and "+" are the parameters, in that example)
arithBOpHelper :: String -> Property
arithBOpHelper opString = 
    forAll (pairsOf integers) $ \((_,s1), (_,s2)) -> typecheck' (s1 ++ opString ++ s2) === Right Integer

boolBOpHelper :: String -> Property
boolBOpHelper opString =
    forAll (pairsOf booleans) $ \((_,s1), (_,s2)) -> typecheck' (s1 ++ opString ++ s2) === Right Boolean