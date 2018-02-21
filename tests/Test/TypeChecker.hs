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

testCase' :: String -> Type -> TestTree
testCase' s t = testCase s (typecheck' s @?= Right t)

testCaseInvert' :: String -> Type -> TestTree
testCaseInvert' s t = testCase ("Fails: " ++ s) (typecheck' s ~?= Right t)

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
            testCase' "begin 17;16;15 end" Integer,
            testCase' "begin true;false end" Boolean
        ],
        testGroup "If"
        [
            testCase' "if true then 1 else 0 end" Integer,
            testCase' "if true then false else true end" Boolean
        ],
        testGroup "Inl"
        [
            testCase' "inl 5 : int + bool" (Union Integer Boolean),
            testCase' "inl (inl 5 : int + bool) : (int + bool) + unit" (Union (Union Integer Boolean) Unit)
        ],
        testGroup "Inr"
        [
            testCase' "inr true : int + bool" (Union Integer Boolean),
            testCase' "inr () : (int + bool) + unit" (Union (Union Integer Boolean) Unit)
        ],
        testGroup "Case"
        [
            testCase' "case inl 5 : int + bool of inl (x : int) -> true | inr (x : bool) -> x end" Boolean
        ],
        testGroup "Fst"
        [
            testCase' "fst (1, 2)" Integer,
            testCase' "fst (true, (1,2))" Boolean
        ],
        testGroup "Snd"
        [
            testCase' "snd (1, 2)" Integer,
            testCase' "snd (true, (1,2))" (Product Integer Integer)
        ],
        testGroup "While"
        [
            testCase' "while true do 5" Integer,
            testCase' "while fst (true, 1) do 5" Integer,
            testCaseInvert' "while snd (true, 1) do 5" Integer
        ],
        testGroup "Let"
        [
            testCase' "let x : int = 5 in x end" Integer,
            testCase' "let x : int * bool = (1, true) in snd x end" Boolean
        ],
        testGroup "LetFun"
        [
            testCase' "let id (x : int) : int = x in id end" (Fun Integer Integer),
            testCase' "let id (x : int) : int = x in id 5 end" Integer
        ],
        testGroup "LetRecFun"
        [

        ],
        testGroup "Fun"
        [
            testCase' "fun (x : int) -> x end" (Fun Integer Integer),
            testCase' "fun (x : int) -> true end" (Fun Integer Boolean)
        ],
        testGroup "Apply"
        [
            testCase' "(fun (x : int) -> true end) 5" Boolean,
            testCaseInvert' "(fun (x : int) -> true end) false" Boolean
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