module Test.Interpreter0
(
    interpreter0Tests,
    interpreter0FileTests
) where

import Test.Tasty
import Test.Tasty.HUnit

import Test.Util

import System.Directory
import System.FilePath
import Control.Monad (forM, liftM)

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
            testCase' "inl bool 5" (Inl $ Integer 5),
            testCase' "inl unit (inl bool 5)" (Inl $ Inl $ Integer 5)
        ],
        testGroup "Inr"
        [
            testCase' "inr int true" (Inr $ Boolean True),
            testCase' "inr (int + bool) ()" (Inr Unit)
        ],
        testGroup "Case"
        [
            testCase' "case inl bool 5 of inl (x : int) -> true | inr (x : bool) -> x end" (Boolean True)
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
            testCase' "while false do 5 end" Unit
        ],
        testGroup "Let"
        [
            testCase' "let x : int = 5 in x end" (Integer 5),
            testCase' "let x : int * bool = (1, true) in snd x end" (Boolean True)
        ],
        testGroup "LetFun"
        [
            testCase' "let id (x : int) : int = x in id end" (Fun "x" (A.Variable "x")),
            testCase' "let id (x : int) : int = x in id 5 end" (Integer 5),
            testCase' "let id (x : int) : int = (x - 1) + (x + 1) in id 5 end" (Integer 10)
        ],
        testGroup "LetRecFun"
        [
            testCase' "let f(x : int) : int = if x = 0 then 1 else x * f (x - 1) end in f 5 end" (Integer 120),
            testCase' "let f(x : int) : int = if x = 0 then 0 else begin f (x - 1) ; f (x - 1) end end in f 5 end" (Integer 0),
            testCase' "let f(x : int) : int = if x < 3 then 1 else f (x - 1) + f (x - 2) end in f 5 end" (Integer 5),
            testCase' "let f(x : int) : int -> int = let g(y : int) : int = x + y in g end in f 4 3 end" (Integer 7)
        ],
        testGroup "Fun"
        [
            testCase' "fun (x : int) -> x end" (Fun "x" (A.Variable "x")),
            testCase' "fun (x : int) -> true end" (Fun "x" (A.Boolean True)),
            testCase' "fun (x : int) -> (fun (y : int) -> x + y end) end 4 3" (Integer 7)
        ],
        testGroup "Apply"
        [
            testCase' "(fun (x : int) -> true end) 5" (Boolean True)
        ]
    ]

interpreter0FileTests :: IO TestTree
interpreter0FileTests = do
                testData <- loadTestFiles
                return $ testGroup "Interpreter 0 File Tests" $
                    map (\(path, prog, res) -> fileTest path prog res) testData

fileTest :: FilePath -> FileContents -> TestResult -> TestTree
fileTest path program result = testCase path $ do
                                    res <- interpret' program
                                    case res of
                                        Left e  -> assertFailure ("Expected: " ++ result ++ "\nGot:      " ++ show e)
                                        Right v -> result @?= show v

type FileContents = String
type TestResult = String
loadTestFiles :: IO [(FilePath, FileContents, TestResult)]
loadTestFiles = do
                    dir <- getCurrentDirectory
                    let testDir = dir </> "tests/slang/"
                    manifestContents <- liftM (map words . lines) $ readFile (testDir </> "manifest.txt")
                    forM manifestContents $ \line -> do
                        let fileName = head line
                            testResult = unwords (tail line)
                            filePath = testDir </> fileName <.> ".slang"
                        program <- readFile filePath
                        return (filePath, program, testResult)