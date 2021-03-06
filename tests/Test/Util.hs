{-# LANGUAGE ScopedTypeVariables #-}

module Test.Util
(
    Program,
    TestResult,
    TestInfo,
    InterpreterName,
    InterpreterInfo,
    testInterpret,
    (~?=),
    alpha,
    alphaNum,
    integers,
    booleans,
    identifiers,
    pairsOf
) where

import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Control.Exception
import Test.QuickCheck.Gen
import Control.Applicative (liftA2)

import Data.Char (toLower)

import Common
import Lexer (reservedTokens)
import Interpreters

-- Useful types for the interpreter tests
type TestResult = String
type TestInfo = (FilePath, Program, TestResult)

type InterpreterName = String
type InterpreterInfo v = (InterpreterName, Interpreter FrontEndError v)

-- Interpret a program, return Nothing if it ran correctly and otherwise return Just the error
testInterpret :: Interpreter FrontEndError Result -> Program -> TestResult -> IO (Either FrontEndError Result)
testInterpret interpreter program result = do
                res <- runInterpreter interpreter program
                case res of
                    Left e  -> return $ Left e
                    Right r -> return $ ifexpr (show r == result) (Right r) (Left $ "Expected: " ++ result ++ "\nGot:      " ++ show r)

-- Inverted version of the Tasty.HUnit assertion @?=
(~?=) :: (Eq a, Show a) => a -> a -> Assertion
x ~?= y = do
        result <- try (x @?= y)
        case result of
            Left (_ :: HUnitFailure) -> return ()
            Right _ -> assertFailure ("Expected != " ++ show y ++ "\nGot         " ++ show x)

makePair :: (a -> b) -> Gen a -> Gen (a, b)
makePair f gen = do
                x <- gen
                return (x, f x)

integers :: Gen (Integer, String)
integers = makePair show arbitrarySizedNatural

booleans :: Gen (Bool, String)
booleans = makePair (map toLower . show) chooseAny

alpha :: Gen Char
alpha = elements $ ['a'..'z'] ++ ['A'..'Z']

alphaNum :: Gen Char
alphaNum = oneof [alpha, elements ['0'..'9']]

identifiers :: Gen String
identifiers = identifiers' `suchThat` (`notElem` reservedTokens)

identifiers' :: Gen String
identifiers' = do h <- alpha
                  t <- listOf alphaNum
                  return $ h:t

pairsOf :: Gen a -> Gen (a, a)
pairsOf g = liftA2 (,) g g