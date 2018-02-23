module Test.Interpreter0
(
    interpreter0Tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import Test.Util

import qualified Interpreters.Ast as A
import Interpreters.Interpreter0
import Parser (parse)
import TypeChecker (typecheck)

interpret' :: String -> IO (Either Error Value)
interpret' s = do
                let parseRes = parse s
                case parseRes of -- Parse string to a syntax tree
                    Left e -> return (Left e)
                    Right parsed -> do
                        let t = typecheck parsed -- Make sure our expressions typecheck
                        case t of
                            Left e  -> return (Left e)
                            Right _ -> interpret (A.translate parsed) -- Translate to an internal AST and interpret

testCase' :: String -> Value -> TestTree
testCase' s v = testCase s $ do
                    r <- interpret' s
                    r @?= Right v

testCaseFails' :: String -> Value -> TestTree
testCaseFails' s v = testCase s $ do
                        r <- interpret' s
                        r ~?= Right v

interpreter0Tests :: TestTree
interpreter0Tests = testGroup "Interpreter0"
    [
        testCase' "()" Unit,
        testCase' "5" (Integer 5),
        testCase' "true" (Boolean True),
        testCase' "(1, 3)" (Pair (Integer 1) (Integer 3)),
        testGroup "Unary Operators"
        [
            testCase' "-5" (Integer $ -5),
            testCase' "--5" (Integer $ 5),
            testCase' "~true" (Boolean False),
            testCase' "~false" (Boolean True)
        ],
        testGroup "Binary Operators"
        [
            testCase' "5+6" (Integer 11),
            testCase' "5-6" (Integer $ -1),
            testCase' "5*6" (Integer 30),
            testCase' "20/4" (Integer 5),
            testCase' "40/6" (Integer 6),
            testCaseFails' "5/0" (Integer 3),
            testCase' "true && true" (Boolean True),
            testCase' "false && true" (Boolean False),
            testCase' "true && false" (Boolean False),
            testCase' "false && false" (Boolean False),
            testCase' "true || true" (Boolean True),
            testCase' "false || true" (Boolean True),
            testCase' "true || false" (Boolean True),
            testCase' "false || false" (Boolean False),
            testCase' "1 = 1" (Boolean True),
            testCase' "1 = 2" (Boolean False),
            testCase' "true = true" (Boolean True),
            testCase' "false = true" (Boolean False),
            testCase' "true = false" (Boolean False),
            testCase' "false = false" (Boolean True),
            testCase' "5 < 4" (Boolean False),
            testCase' "0 < 2" (Boolean True)
        ],
        testGroup "Pair"
        [
            testCase' "(1, 2)" (Pair (Integer 1) (Integer 2)),
            testCase' "(1, true)" (Pair (Integer 1) (Boolean True))
        ],
        testGroup "Ref"
        [
            testCase' "ref 5" (Ref "$0"),
            testCase' "ref true" (Ref "$0")
        ],
        testGroup "Deref"
        [
            testCase' "!ref 5" (Integer 5),
            testCase' "!(ref true)" (Boolean True)
        ],
        testGroup "Sequence"
        [
            testCase' "begin 5 ; true end" (Boolean True),
            testCase' "begin 5 ; true ; false end" (Boolean False),
            testCase' "begin (1,4) ; () end" Unit
        ],
        testGroup "If"
        [
            testCase' "if 4 < 5 then false else true end" (Boolean False),
            testCase' "if true then 1 else 0 end" (Integer 1)
        ],
        testGroup "Inl"
        [
            testCase' "inl 5 : int + bool" (Inl $ Integer 5),
            testCase' "inl (inl 5 : int + bool) : (int + bool) + unit" (Inl $ Inl $ Integer 5)
        ],
        testGroup "Inr"
        [
            testCase' "inr true : int + bool" (Inr $ Boolean True),
            testCase' "inr () : (int + bool) + unit" (Inr Unit)
        ],
        testGroup "Case"
        [
            testCase' "case inl 5 : int + bool of inl (x : int) -> true | inr (x : bool) -> x end" (Boolean True)
        ],
        testGroup "Fst"
        [
            testCase' "fst (1, 2)" (Integer 1),
            testCase' "fst (true, (1,2))" (Boolean True)
        ],
        testGroup "Snd"
        [
            testCase' "snd (1, 2)" (Integer 2),
            testCase' "snd (true, (1,2))" (Pair (Integer 1) (Integer 2))
        ],
        testGroup "While"
        [
            testCase' "while false do 5" Unit
        ],
        testGroup "Let"
        [
            testCase' "let x : int = 5 in x end" (Integer 5),
            testCase' "let x : int * bool = (1, true) in snd x end" (Boolean True)
        ],
        testGroup "LetFun"
        [
            testCase' "let id (x : int) : int = x in id end" (Fun $ A.Lambda "x" (A.Variable "x")),
            testCase' "let id (x : int) : int = x in id 5 end" (Integer 5),
            testCase' "let id (x : int) : int = (x - 1) + (x + 1) in id 5 end" (Integer 10)
        ],
        testGroup "LetRecFun"
        [
            testCase' "let rec f(x : int) : int = if x = 0 then 1 else x * f (x - 1) end in f 5 end" (Integer 120),
            testCase' "let rec f(x : int) : int = if x = 0 then 0 else begin f (x - 1) ; f (x - 1) end end in f 5 end" (Integer 0),
            testCase' "let rec f(x : int) : int = if x < 3 then 1 else f (x - 1) + f (x - 2) end in f 5 end" (Integer 5)
        ],
        testGroup "Fun"
        [
            testCase' "fun (x : int) -> x end" (Fun $ A.Lambda "x" (A.Variable "x")),
            testCase' "fun (x : int) -> true end" (Fun $ A.Lambda "x" (A.Boolean True))
        ],
        testGroup "Apply"
        [
            testCase' "(fun (x : int) -> true end) 5" (Boolean True)
        ]
    ]