module Test.Regex.Internal.NFA where

import Test.Tasty
import Test.Tasty.HUnit

import Regex.Internal.NFA
import Regex.Internal.Graph
import qualified Data.Set as S

nfaTests :: TestTree
nfaTests = testGroup "NFA Tests"
    [
        testGroup "union"
        [
            testCase "[empty]" $
                union [empty]
                @?=
                NFA (buildGraph [Node (), Node (), Node (), Node ()]
                                [(0, Epsilon, 1), (1, Epsilon, 2), (2, Epsilon, 3)])
                    (S.singleton 0)
                    (S.singleton 3),
            
            testCase "[custom, custom]" $
                union [NFA (buildGraph [Node ()] []) (S.singleton 0) S.empty,
                       NFA (buildGraph [Node ()] []) S.empty (S.singleton 0)]
                @?=
                NFA (buildGraph [Node (), Node (), Node (), Node ()]
                                [(0, Epsilon, 1), (2, Epsilon, 3)])
                    (S.singleton 0)
                    (S.singleton 3),

            testCase "[custom, custom]" $
                union [NFA (buildGraph [Node ()] []) (S.singleton 0) S.empty,
                       NFA (buildGraph [Node (), Node ()] [(0, Epsilon, 1)]) (S.singleton 0) (S.singleton 1)]
                @?=
                NFA (buildGraph [Node (), Node (), Node (), Node (), Node ()]
                                [(0, Epsilon, 1), (0, Epsilon, 2), (2, Epsilon, 3), (3, Epsilon, 4)])
                    (S.singleton 0)
                    (S.singleton 4),

            testCase "[custom, custom]" $
                union [NFA (buildGraph [Node ()] []) (S.singleton 0) S.empty,
                       NFA (buildGraph [Node (), Node ()] [(0, Epsilon, 1)]) (S.singleton 0) (S.singleton 1)]
                @?=
                NFA (buildGraph [Node (), Node (), Node (), Node (), Node ()]
                                [(0, Epsilon, 1), (0, Epsilon, 2), (2, Epsilon, 3), (3, Epsilon, 4)])
                    (S.singleton 0)
                    (S.singleton 4)
        ],

        testGroup "concatenate"
        [
            
        ]
    ]