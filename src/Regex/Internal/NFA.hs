module Regex.Internal.NFA where

import qualified Regex.Internal.Graph as G
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Map as M

data NFAInput = Epsilon | Symbol Char deriving (Show, Eq)

-- Impose an ordering on NFAInputs so they can be used in Maps
instance Ord NFAInput where
    Epsilon <= _           = True
    Symbol _ <= Epsilon    = False
    Symbol c1 <= Symbol c2 = c1 <= c2

data NFA = NFA
    {
        graph :: G.AdjList () NFAInput, -- In an NFA, we don't have any labels on the nodes
        starts :: S.Set G.NodeIndex,
        acceptors :: S.Set G.NodeIndex
    } deriving Show


-- merge puts the NFAs into the same "address space", and joins them with the given edges.
-- The first edge list is the list of edges from the 1st NFA to the 2nd, and the 2nd list is the opposite.
-- This is a helper function for the combination functions below
mergeWithEdges :: NFA -> [G.Edge NFAInput] -> NFA -> [G.Edge NFAInput] -> NFA
mergeWithEdges (NFA g1 s1 a1) es12 (NFA g2 s2 a2) es21 =
    let
        g1Length = V.length g1

        -- Get the joining edge list, translating the nodes in the 2nd NFA
        es = (map (\(s,x,d) -> (s,x,d + g1Length)) es12) ++ (map (\(s,x,d) -> (s + g1Length,x,d)) es21)

        -- Shift all the indices in the 2nd NFA's details to the position of its nodes in the appended array        
        g2' = V.map (\(node, neighbours) -> (node, M.map (+g1Length) neighbours)) g2
        gr = G.insertEdges (g1 V.++ g2') es -- Add the extra edges
        st = S.union s1 $ S.map (+g1Length) s2
        ac = S.union a1 $ S.map (+g1Length) a2
    in
        NFA gr st ac

-- Specialisation of mergeWithEdges for when the NFAs are disjoint
merge :: NFA -> NFA -> NFA
merge n1 n2 = mergeWithEdges n1 [] n2 []

-- These rules are for combining NFA-e's, as given here: https://en.wikipedia.org/wiki/Thompson%27s_construction#Rules

-- An NFA with 2 nodes and 1 edge, from the start state to an accepting state on the given input
hop :: NFAInput -> NFA
hop i = NFA g (S.singleton 0) (S.singleton 1)
            where g = G.buildGraph [G.Node (), G.Node ()] [(0, i, 1)]

-- Matches the empty input
empty :: NFA
empty = hop Epsilon

-- Matches a single symbol
single :: Char -> NFA
single c = hop (Symbol c)

-- Union of expressions
union :: [NFA] -> NFA
union ns =
    let
        start = NFA (G.buildGraph [(G.Node ())] []) S.empty S.empty
        end = NFA (G.buildGraph [(G.Node ())] []) S.empty S.empty

        middle = foldl1 merge ns -- Disjoint merge the given NFAs

        -- Construct edges from the start to every start state in the merged NFA
        startEdges = map (\i -> (0, Epsilon, i)) (S.toList $ starts middle)
        endEdges = map (\i -> (i, Epsilon, 0)) (S.toList $ acceptors middle)

        g = mergeWithEdges start startEdges middle [] -- Add the start state
        g' = mergeWithEdges g [] end endEdges -- Add the accepting state

        startIndex = 0
        endIndex = V.length (graph g') - 1
    in
        NFA (graph g') (S.singleton startIndex) (S.singleton endIndex)