module Test.TypeChecker where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Test.Util

import TypeChecker
import Common
import Parser
import Parser.Types

typecheck' :: String -> Either FrontEndError Type
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
        testCase' "()" Unit,
        testProperty "Integer" $ forAll integers $ \(_,s) -> typecheck' s === (Right Integer),
        testProperty "Boolean" $ forAll booleans $ \(_,s) -> typecheck' s === (Right Boolean),
        testCase' "(1, 2)" (Product Integer Integer),
        testCase' "(1, true)" (Product Integer Boolean),
        testCase' "ref 5" (Ref Integer),
        testCase' "ref true" (Ref Boolean),
        testCase' "!ref 5" Integer,
        testCase' "!(ref true)" Boolean
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
            testProperty "OpDiv" (arithBOpHelper "/")
        ],
        testGroup "BoolBinaryOp"
        [
            testProperty "OpAnd" (boolBOpHelper "&&"),
            testProperty "OpOr" (boolBOpHelper "||")
        ],
        testGroup "BinaryOp"
        [
            testCase' "17 = 18" Boolean,
            testCase' "true = false" Boolean,
            testCase' "() = ()" Boolean,
            testCase' "5 < 4" Boolean,
            testCaseInvert' "true < 4" Boolean,
            testCase' "let x : int ref = ref 0 in x := !x + 1 end" Unit
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
            testCase' "inl bool 5" (Union Integer Boolean),
            testCase' "inl unit (inl bool 5)" (Union (Union Integer Boolean) Unit)
        ],
        testGroup "Inr"
        [
            testCase' "inr int true" (Union Integer Boolean),
            testCase' "inr (int + bool) ()" (Union (Union Integer Boolean) Unit)
        ],
        testGroup "Case"
        [
            testCase' "case inl bool 5 of inl (x : int) -> true | inr (x : bool) -> x end" Boolean
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
            testCase' "while true do 5 end" Integer,
            testCase' "while fst (true, 1) do 5 end" Integer,
            testCaseInvert' "while snd (true, 1) do 5 end" Integer
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
            testCase' "let f(x : int) : int = if x = 0 then 1 else x * f (x - 1) end in f 5 end" Integer,
            testCase' "let f(x : int) : int = if x < 3 then 1 else f (x - 1) + f (x - 2) end in f 5 end" Integer
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