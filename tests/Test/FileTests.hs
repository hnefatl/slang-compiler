module Test.FileTests
(
    fileTests
) where

import Test.Tasty
import Test.Tasty.HUnit

import Test.Util

import Interpreters

import System.Directory
import System.FilePath
import Control.Monad (forM, when, liftM)
import Data.Either (isLeft)

fileTests :: [InterpreterInfo Result] -> IO TestTree
fileTests interpreters = do
    tests <- testInterpreters interpreters
    return $ testGroup "File Tests" tests

testInterpreters :: [InterpreterInfo Result] -> IO [TestTree]
testInterpreters interpreters = do
        testData <- loadTestFiles
        return $ map (testFile interpreters) testData

testFile :: [InterpreterInfo Result] -> TestInfo -> TestTree
testFile interpreters (path, program, result) =
        testGroup (takeBaseName path) $
            map (testInterpreter program result) interpreters

testInterpreter :: Program -> TestResult -> InterpreterInfo Result -> TestTree
testInterpreter program result (name, tester) =
        testCase name $ do
            res <- testInterpret tester program result
            when (isLeft res) (assertFailure $ "Expected: " ++ result ++ "\nGot:      " ++ show res)

loadTestFiles :: IO [TestInfo]
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