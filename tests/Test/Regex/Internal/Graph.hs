module Test.Regex.Internal.Graph where

import Test.Tasty
import Test.Tasty.HUnit

import Regex.Internal.Graph
import qualified Data.Vector as V
import qualified Data.Map as M

graphTests :: TestTree
graphTests = testGroup "Graph Tests"
    [
        testGroup "buildGraph"
        [
            testCase "[Node 0] []" $
                (buildGraph [Node 0] [] :: AdjList Int ())
                @?=
                V.fromList [(Node 0, M.empty)],
        
            testCase "[Node 0, Node 1] [(0, (), 1)]" $
                (buildGraph [Node 0, Node 1] [(0, (), 1)] :: AdjList Int ())
                @?=
                V.fromList [(Node 0, M.singleton () [1]), (Node 1, M.empty)]
        ],

        testGroup "vectorUpdate"
        [
            testCase "id 0 [0]" $
                vectorUpdate id 0 (V.fromList [0 :: Int])
                @?=
                V.fromList [0],
                    
            testCase "(*2) 1 [0,1]" $
                vectorUpdate (*2) 1 (V.fromList [0, 1] :: V.Vector Int)
                @?=
                V.fromList [0, 2]
        ]
    ]