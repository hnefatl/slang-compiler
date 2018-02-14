module Test.Parser.Parser where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Parser.Parser
import Parser.Expressions

parserTests :: TestTree
parserTests = testGroup "Parser" $
    [
        testCase "17 + 4" $ parse "17 + 4" @?= Right (BinaryOp Add (SimpleExpr $ Integer 17) (SimpleExpr $ Integer 4))
    ]