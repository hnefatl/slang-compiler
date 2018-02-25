module Test.FileTests
(
    fileTests,
    InterpreterName,
    InterpreterTester,
    InterpreterInfo,
    interpreterWrapper
) where

import Test.Tasty
import Test.Tasty.HUnit

import Common

import System.Directory
import System.FilePath
import Control.Monad (forM, when, liftM)
import Data.Maybe (isJust)

type FileContents = String
type TestResult = String
type TestInfo = (FilePath, FileContents, TestResult)

type InterpreterName = String
type InterpreterTester = FileContents -> TestResult -> IO (Maybe String)
type InterpreterInfo = (InterpreterName, InterpreterTester)

fileTests :: [InterpreterInfo] -> IO TestTree
fileTests interpreters = do
    tests <- testInterpreters interpreters
    return $ testGroup "File Tests" tests

interpreterWrapper :: Show a => (String -> IO (Either Error a)) -> (FileContents -> TestResult -> IO (Maybe Error))
interpreterWrapper interpreter program result = do
            output <- interpreter program
            case output of
                Left err  -> return $ Just err
                Right val -> return $ ifexpr (show val == result) Nothing (Just $ "Got " ++ show val)


testInterpreters :: [InterpreterInfo] -> IO [TestTree]
testInterpreters interpreters = do
        testData <- loadTestFiles
        return $ map (testFile interpreters) testData

testFile :: [InterpreterInfo] -> TestInfo -> TestTree
testFile interpreters (path, program, result) =
        testGroup (takeBaseName path) $
            map (testInterpreter program result) interpreters

testInterpreter :: FileContents -> TestResult -> InterpreterInfo -> TestTree
testInterpreter program result (name, tester) =
        testCase name $ do
            res <- tester program result
            when (isJust res) (assertFailure $ "Expected: " ++ result ++ "\nGot:      " ++ show res)

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