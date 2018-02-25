module Test.Interpreters
(
    interpreterTests
) where

import Test.Tasty
import Test.Tasty.HUnit

import Test.Util

import Interpreters

testCase' :: Program -> Result -> InterpreterInfo Result -> TestTree
testCase' program result (name, interpreter) = testCase name $ do
            res <- testInterpret interpreter program (show result)
            case res of
                Left e  -> assertFailure e
                Right _ -> return ()

testCaseFails' :: Program -> Result -> InterpreterInfo Result -> TestTree
testCaseFails' program result (name, interpreter) = testCase name $ do
            res <- testInterpret interpreter program (show result)
            case res of
                Left _  -> return ()
                Right r -> assertFailure ("Got " ++ show r)

interpreterTests :: [InterpreterInfo Result] -> TestTree
interpreterTests interpreters =
    let 
        test program result = testGroup program $ map (testCase' program result) interpreters
        testFails program result = testGroup program $ map (testCaseFails' program result) interpreters
    in testGroup "Interpreter Tests"
    [
        test "()" Unit,
        test "5" (Integer 5),
        test "true" (Boolean True),
        test "(1, 3)" (Pair (Integer 1) (Integer 3)),
        testGroup "Unary Operators"
        [
            test "-5" (Integer $ -5),
            test "--5" (Integer $ 5),
            test "~true" (Boolean False),
            test "~false" (Boolean True)
        ],
        testGroup "Binary Operators"
        [
            test "5+6" (Integer 11),
            test "5-6" (Integer $ -1),
            test "5*6" (Integer 30),
            test "20/4" (Integer 5),
            test "40/6" (Integer 6),
            testFails "5/0" (Integer 0),
            test "true && true" (Boolean True),
            test "false && true" (Boolean False),
            test "true && false" (Boolean False),
            test "false && false" (Boolean False),
            test "true || true" (Boolean True),
            test "false || true" (Boolean True),
            test "true || false" (Boolean True),
            test "false || false" (Boolean False),
            test "1 = 1" (Boolean True),
            test "1 = 2" (Boolean False),
            test "true = true" (Boolean True),
            test "false = true" (Boolean False),
            test "true = false" (Boolean False),
            test "false = false" (Boolean True),
            test "5 < 4" (Boolean False),
            test "0 < 2" (Boolean True)
        ],
        testGroup "Pair"
        [
            test "(1, 2)" (Pair (Integer 1) (Integer 2)),
            test "(1, true)" (Pair (Integer 1) (Boolean True))
        ],
        testGroup "Ref"
        [
            -- Can't test ref values as they're not a valid Result from an interpreter
            --testCond "ref 5" (\v -> case v of Ref _ -> True ; _ -> False) "Didn't get a reference value",
            --testCond "ref true" (\v -> case v of Ref _ -> True ; _ -> False) "Didn't get a reference value"
        ],
        testGroup "Deref"
        [
            test "!ref 5" (Integer 5),
            test "!(ref true)" (Boolean True),
            test "let x : int ref = ref 0 in begin x := !x + 1 ; !x end end" (Integer 1)
        ],
        testGroup "Sequence"
        [
            test "begin 5 ; true end" (Boolean True),
            test "begin 5 ; true ; false end" (Boolean False),
            test "begin (1,4) ; () end" Unit
        ],
        testGroup "If"
        [
            test "if 4 < 5 then false else true end" (Boolean False),
            test "if true then 1 else 0 end" (Integer 1)
        ],
        testGroup "Inl"
        [
            test "inl bool 5" (Inl $ Integer 5),
            test "inl unit (inl bool 5)" (Inl $ Inl $ Integer 5)
        ],
        testGroup "Inr"
        [
            test "inr int true" (Inr $ Boolean True),
            test "inr (int + bool) ()" (Inr Unit)
        ],
        testGroup "Case"
        [
            test "case inl bool 5 of inl (x : int) -> true | inr (x : bool) -> x end" (Boolean True)
        ],
        testGroup "Fst"
        [
            test "fst (1, 2)" (Integer 1),
            test "fst (true, (1,2))" (Boolean True)
        ],
        testGroup "Snd"
        [
            test "snd (1, 2)" (Integer 2),
            test "snd (true, (1,2))" (Pair (Integer 1) (Integer 2))
        ],
        testGroup "While"
        [
            test "while false do 5 end" Unit,
            test("let x : int ref = ref 5 in let y : int ref = ref 1 in begin while 0 < !x do (begin y := !y * !x ; x := !x - 1 end) end ; !y end end end") (Integer 120)
        ],
        testGroup "Let"
        [
            test "let x : int = 5 in x end" (Integer 5),
            test "let x : int * bool = (1, true) in snd x end" (Boolean True)
        ],
        testGroup "LetFun"
        [
            test "let id (x : int) : int = x in id 5 end" (Integer 5),
            test "let id (x : int) : int = (x - 1) + (x + 1) in id 5 end" (Integer 10)
        ],
        testGroup "LetRecFun"
        [
            test "let f(x : int) : int = if x = 0 then 1 else x * f (x - 1) end in f 5 end" (Integer 120),
            test "let f(x : int) : int = if x = 0 then 0 else begin f (x - 1) ; f (x - 1) end end in f 5 end" (Integer 0),
            test "let f(x : int) : int = if x < 3 then 1 else f (x - 1) + f (x - 2) end in f 5 end" (Integer 5),
            test "let f(x : int) : int -> int = let g(y : int) : int = x + y in g end in f 4 3 end" (Integer 7)
        ],
        testGroup "Fun"
        [
            test "fun (x : bool) -> x end false" (Boolean False),
            test "fun (x : int) -> true end 5" (Boolean True),
            test "fun (x : int) -> (fun (y : int) -> x + y end) end 4 3" (Integer 7)
        ],
        testGroup "Apply"
        [
            test "(fun (x : int) -> true end) 5" (Boolean True)
        ]
    ]