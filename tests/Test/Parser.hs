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
        testCase' "()" (SimpleExpr Unit),
        testProperty "Integer" $ forAll integers $ \(i,s) -> parse s === (Right . SimpleExpr . Integer) i,
        testProperty "Boolean" $ forAll booleans $ \(b,s) -> parse s === (Right . SimpleExpr . Boolean) b,
        testProperty "Identifer" $ forAll identifiers $ \i -> parse i == (Right . SimpleExpr . Identifier) i,
        testCase' "(1, 2)" (SimpleExpr $ Pair (SimpleExpr $ Integer 1) (SimpleExpr $ Integer 2)),
        testCase' "(1, true)" (SimpleExpr $ Pair (SimpleExpr $ Integer 1) (SimpleExpr $ Boolean True)),
        testCase' "ref 5" (SimpleExpr $ Ref $ Integer 5),
        testCase' "ref true" (SimpleExpr $ Ref $ Boolean True),
        testCase' "!ref 5" (SimpleExpr $ Deref $ Ref $ Integer 5),
        testCase' "!(ref true)" (SimpleExpr $ Deref $ Expr $ SimpleExpr $ Ref $ Boolean True)
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
            testProperty "OpDiv" (arithBOpHelper OpDiv "/")
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
            testCase' "() = ()" (BinaryOp OpEqual (SimpleExpr Unit) (SimpleExpr Unit)),
            testCase' "5 < 4" (BinaryOp OpLess (SimpleExpr $ Integer 5) (SimpleExpr $ Integer 4)),
            testCase' "true < 4" (BinaryOp OpLess (SimpleExpr $ Boolean True) (SimpleExpr $ Integer 4)),
            testCase' "x := !x" (BinaryOp OpAssign (SimpleExpr $ Identifier "x") (SimpleExpr $ Deref $ Identifier "x")),
            testCase' "x := !x + 1" (BinaryOp OpAssign (SimpleExpr $ Identifier "x") (ArithBinaryOp OpAdd (SimpleExpr $ Deref $ Identifier "x") (SimpleExpr $ Integer 1)))
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
            testCase' "inl bool 5" (Inl (SimpleExpr $ Integer 5) T.Boolean),
            testCase' "inl unit (inl bool 5)" (Inl (SimpleExpr $ Expr $ Inl (SimpleExpr $ Integer 5) T.Boolean) T.Unit)
        ],
        testGroup "Inr"
        [
            testCase' "inr int true" (Inr (SimpleExpr $ Boolean True) T.Integer),
            testCase' "inr (int + bool) ()" (Inr (SimpleExpr Unit) (T.Union T.Integer T.Boolean))
        ],
        testGroup "Case"
        [
            testCase' "case inl bool 5 of inl (x : int) -> true | inr (x : bool) -> x end" (Case (Inl (SimpleExpr $ Integer 5) T.Boolean) (Fun "x" T.Integer $ SimpleExpr $ Boolean True) (Fun "x" T.Boolean $ SimpleExpr $ Identifier "x"))
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
            testCase' "while true do 5 end" (While (SimpleExpr $ Boolean True) (SimpleExpr $ Integer 5)),
            testCase' "while fst (true, 1) do 5 end" (While (Fst $ SimpleExpr $ Pair (SimpleExpr $ Boolean True) (SimpleExpr $ Integer 1)) (SimpleExpr $ Integer 5))
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
        testGroup "LetFun Recursive"
        [
            testCase' "let f(x : int) : int = if x = 0 then 1 else x * f x end in f 5 end" (LetFun "f" (Fun "x" T.Integer (If (BinaryOp OpEqual (SimpleExpr $ Identifier "x") (SimpleExpr $ Integer 0)) (SimpleExpr $ Integer 1) (ArithBinaryOp OpMul (SimpleExpr $ Identifier "x") (Application (SimpleExpr $ Identifier "f") (Identifier "x"))))) T.Integer (Application (SimpleExpr $ Identifier "f") (Integer 5))),
            testCase' "let f(x : int) : int = if x < 1 then 1 else f (x - 1) + f (x - 2) end in f 5 end" $
                    LetFun "f"
                            (Fun "x" T.Integer $
                                If (BinaryOp OpLess (SimpleExpr $ Identifier "x") (SimpleExpr $ Integer 1))
                                    (SimpleExpr $ Integer 1)
                                    (ArithBinaryOp OpAdd 
                                        (Application (SimpleExpr $ Identifier "f") (Expr $ ArithBinaryOp OpSub (SimpleExpr $ Identifier "x") (SimpleExpr $ Integer 1)))
                                        (Application (SimpleExpr $ Identifier "f") (Expr $ ArithBinaryOp OpSub (SimpleExpr $ Identifier "x") (SimpleExpr $ Integer 2)))))
                            T.Integer
                        (Application (SimpleExpr $ Identifier "f") (Integer 5))
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