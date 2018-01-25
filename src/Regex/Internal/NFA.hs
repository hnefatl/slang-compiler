module Regex.Internal.NFA where

import qualified Regex.Internal.Graph as G
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Map as M
import qualified Control.Monad.Trans.State as ST
import Data.Maybe (fromMaybe)
import Data.List (nub)
import Control.Monad (liftM)

data NFAInput = Epsilon | Symbol Char deriving (Show, Eq)

-- Impose an ordering on NFAInputs so they can be used in Maps
instance Ord NFAInput where
    Epsilon <= _           = True
    Symbol _ <= Epsilon    = False
    Symbol c1 <= Symbol c2 = c1 <= c2

data NFA = NFA
    {
        graph :: G.AdjList () NFAInput, -- Don't have labels on nodes - otherwise constructions are messy
        starts :: S.Set G.NodeIndex,
        acceptors :: S.Set G.NodeIndex
    } deriving (Show, Eq)


-- merge puts the NFAs into the same "address space", and joins them with the given edges.
-- The first edge list is the list of edges from the 1st NFA to the 2nd, and the 2nd list is the opposite.
-- This is a helper function for the combination functions below
mergeWithEdges :: NFA -> [G.Edge NFAInput] -> NFA -> [G.Edge NFAInput] -> NFA
mergeWithEdges (NFA g1 s1 a1) es12 (NFA g2 s2 a2) es21 =
    let
        g1Length = V.length g1

        -- Get the joining edge list, translating the nodes in the 2nd NFA to where they'll be in the appended array
        es = (map (\(s,x,d) -> (s,x,d + g1Length)) es12) ++ (map (\(s,x,d) -> (s + g1Length,x,d)) es21)

        -- Shift all the indices in the 2nd NFA's details to the position of its nodes in the appended array        
        g2' = V.map (\(node, neighbours) -> (node, M.map (map (+ g1Length)) neighbours)) g2
        gr = G.insertEdges (g1 V.++ g2') es -- Add the extra edges
        st = S.union s1 $ S.map (+ g1Length) s2
        ac = S.union a1 $ S.map (+ g1Length) a2
    in
        NFA gr st ac

-- Specialisation of mergeWithEdges for when the NFAs are disjoint
merge :: NFA -> NFA -> NFA
merge n1 n2 = mergeWithEdges n1 [] n2 []



-- An NFA with a single state, that's a start state
singleStart :: NFA
singleStart = NFA (G.singleton ()) (S.singleton 0) S.empty

-- An NFA with a single state, that's an accepting state
singleAccept :: NFA
singleAccept = NFA (G.singleton ()) S.empty (S.singleton 0)

-- Adds a new start state that's connected by epsilon transitions to all the original start states
addStart :: NFA -> NFA
addStart (NFA g s a) = mergeWithEdges singleStart connectors (NFA g S.empty a) []
        where
            connectors = [ (0, Epsilon, i) | i <- S.toList s ] -- Edges from new start to existing starts

-- Adds a new accepting state that's connected to by the original accepting states by epsilon transitions
addAcceptor :: NFA -> NFA
addAcceptor (NFA g s a) = mergeWithEdges (NFA g s S.empty) connectors singleAccept []
        where
            connectors = [ (i, Epsilon, 0) | i <- S.toList a] -- Edges from existing acceptors to new acceptor



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
        middle = foldl1 merge ns -- Disjoint merge the given NFAs
        g = addAcceptor $ addStart middle -- Add the start and end states
        endIndex = V.length (graph g) - 1 -- The accepting node is the last in the vector
    in
        NFA (graph g) (S.singleton 0) (S.singleton endIndex)

-- Concatenation of expressions
concatenate :: NFA -> NFA -> NFA
concatenate (NFA g1 s1 a1) (NFA g2 s2 a2) =
    let
        -- Construct edges from 1st NFA's accept states to 2nd NFA's start states
        connectors = [ (s, Epsilon, d) | s <- S.toList a1, d <- S.toList s2 ]
    in
        -- Merge 1st NFA minus with the accepting flags removed with 2nd NFA with the start flags removed,
        -- inserting the new connector edges
        mergeWithEdges (NFA g1 s1 S.empty) connectors (NFA g2 S.empty a2) []

-- Kleene star on an expression
-- This varies from the version on the webpage as this is nicer to implement
-- Diagram is KleeneStar.png in the top-level directory of this repository
star :: NFA -> NFA
star n =
    let
        -- Add the inner start/accept states and the edge between them
        (NFA g1 s1 a1) = addAcceptor $ addStart n
        g1' = G.insertEdge g1 (V.length g1 - 1, Epsilon, 0)

        -- Add the outer start/accept states and the edge between them
        (NFA g2 s2 a2) = addAcceptor $ addStart (NFA g1' s1 a1)
        g2' = G.insertEdge g2 (0, Epsilon, V.length g2 - 1)
    in
        NFA g2' s2 a2


-- NEEDS TESTING
-- Shouldn't be too bad to write a few unit tests
epsilonClosure :: NFA -> V.Vector [G.NodeIndex]
epsilonClosure (NFA g _ _) = V.fromList $ ST.evalState (mapM (epsilonClosureCalc g) [0..stateCount-1]) startState
        where stateCount = V.length g
              startState = (V.replicate stateCount Nothing)

epsilonClosureCalc :: G.AdjList () NFAInput -> Int -> ST.State (V.Vector (Maybe [G.NodeIndex])) [G.NodeIndex]
epsilonClosureCalc g i = do
                    memo <- ST.gets (V.! i) -- Get the memoised state if it exists
                    case memo of
                        Just closure -> return closure -- Already computed
                        Nothing -> do
                            -- Compute the epsilon closure of all Epsilon-reachable neighbours
                            let epsilonNeighbours = fromMaybe [] $ M.lookup Epsilon (snd $ g V.! i)
                            closure <- liftM (nub . concat) $ mapM (epsilonClosureCalc g) epsilonNeighbours

                            -- Memoise the result
                            ST.modify $ (V.// [(i, Just closure)])
                            return closure