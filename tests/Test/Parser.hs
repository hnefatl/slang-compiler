module Test.Parser where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Parser
import Parser.Expressions
import qualified Parser.Types as T

import Test.Util

testCase' :: String -> Expr -> TestTree
testCase' s e = testCase s (parse s @?= Right e)

testCaseInvert' :: String -> Expr -> TestTree
testCaseInvert' s e = testCase s (parse s ~?= Right e)

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
            testCase' "17 = 18" (BinaryOp OpEqual (SimpleExpr $ Integer 17) (SimpleExpr $ Integer 18)),
            testCase' "true = false" (BinaryOp OpEqual (SimpleExpr $ Boolean True) (SimpleExpr $ Boolean False)),
            testCase' "() = ()" (BinaryOp OpEqual (SimpleExpr Unit) (SimpleExpr Unit))
        ],
        testGroup "Sequence"
        [
            testCase' "begin 17;16;15 end" (Sequence [SimpleExpr (Integer 17), SimpleExpr (Integer 16), SimpleExpr (Integer 15)]),
            testCase' "begin true;false end" (Sequence [SimpleExpr (Boolean True), SimpleExpr (Boolean False)])
        ],
        testGroup "If"
        [
            testCase' "if true then 1 else 0 end" (If (SimpleExpr $ Boolean True) (SimpleExpr $ Integer 1) (SimpleExpr $ Integer 0)),
            testCase' "if true then false else true end" (If (SimpleExpr $ Boolean True) (SimpleExpr $ Boolean False) (SimpleExpr $ Boolean True))
        ],
        testGroup "Inl"
        [
            testCase' "inl 5 : int + bool" (Inl (SimpleExpr $ Integer 5) (T.Union T.Integer T.Boolean)),
            testCase' "inl (inl 5 : int + bool) : (int + bool) + unit" (Inl (SimpleExpr $ Expr $ Inl (SimpleExpr $ Integer 5) (T.Union T.Integer T.Boolean)) (T.Union (T.Union T.Integer T.Boolean) T.Unit))
        ],
        testGroup "Inr"
        [
            testCase' "inr true : int + bool" (Inr (SimpleExpr $ Boolean True) (T.Union T.Integer T.Boolean)),
            testCase' "inr () : (int + bool) + unit" (Inr (SimpleExpr Unit) (T.Union (T.Union T.Integer T.Boolean) T.Unit))
        ],
        testGroup "Case"
        [
            testCase' "case inl 5 : int + bool of inl (x : int) -> true | inr (x : bool) -> x end" (Case (Inl (SimpleExpr $ Integer 5) (T.Union T.Integer T.Boolean)) (Fun "x" T.Integer $ SimpleExpr $ Boolean True) (Fun "x" T.Boolean $ SimpleExpr $ Identifier "x"))
        ],
        testGroup "Fst"
        [
            testCase' "fst (1, 2)" (Fst $ SimpleExpr $ Pair (SimpleExpr $ Integer 1) (SimpleExpr $ Integer 2)),
            testCase' "fst (true, (1,2))" (Fst $ SimpleExpr $ Pair (SimpleExpr $ Boolean True) (SimpleExpr $ Pair (SimpleExpr $ Integer 1) (SimpleExpr $ Integer 2)))
        ],
        testGroup "Snd"
        [
            testCase' "snd (1, 2)" (Snd $ SimpleExpr $ Pair (SimpleExpr $ Integer 1) (SimpleExpr $ Integer 2)),
            testCase' "snd (true, (1,2))" (Snd $ SimpleExpr $ Pair (SimpleExpr $ Boolean True) (SimpleExpr $ Pair (SimpleExpr $ Integer 1) (SimpleExpr $ Integer 2)))
        ],
        testGroup "While"
        [
            testCase' "while true do 5" (While (SimpleExpr $ Boolean True) (SimpleExpr $ Integer 5)),
            testCase' "while fst (true, 1) do 5" (While (Fst $ SimpleExpr $ Pair (SimpleExpr $ Boolean True) (SimpleExpr $ Integer 1)) (SimpleExpr $ Integer 5))
        ],
        testGroup "Let"
        [
            testCase' "let x : int = 5 in x end" (Let "x" T.Integer (SimpleExpr $ Integer 5) (SimpleExpr $ Identifier "x")),
            testCase' "let x : int * bool = (1, true) in snd x end" (Let "x" (T.Product T.Integer T.Boolean) (SimpleExpr $ Pair (SimpleExpr $ Integer 1) (SimpleExpr $ Boolean True)) (Snd $ SimpleExpr $ Identifier "x"))
        ],
        testGroup "LetFun"
        [
            testCase' "let id (x : int) : int = x in id end" (LetFun "id" (Fun "x" T.Integer (SimpleExpr $ Identifier "x")) T.Integer (SimpleExpr $ Identifier "id")),
            testCase' "let id (x : int) : int = x in id 5 end" (LetFun "id" (Fun "x" T.Integer (SimpleExpr $ Identifier "x")) T.Integer (Application (SimpleExpr $ Identifier "id") (Integer 5)))
        ],
        testGroup "LetRecFun"
        [

        ],
        testGroup "Fun"
        [
            testCase' "fun (x : int) -> x end" (Fun "x" T.Integer (SimpleExpr $ Identifier "x")),
            testCase' "fun (x : int) -> true end" (Fun "x" T.Integer (SimpleExpr $ Boolean True))
        ],
        testGroup "Apply"
        [
            testCase' "(fun (x : int) -> true end) 5" (Application (SimpleExpr $ Expr $ Fun "x" T.Integer (SimpleExpr $ Boolean True)) (Integer 5)),
            testCase' "(fun (x : bool) -> x end) false" (Application (SimpleExpr $ Expr $ Fun "x" T.Boolean (SimpleExpr $ Identifier "x")) (Boolean False))
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