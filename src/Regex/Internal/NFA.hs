module Regex.Internal.NFA where

import qualified Regex.Internal.Graph as G
import qualified Data.Set as S

data NFAInput = Epsilon | Symbol Char deriving (Show, Eq)

-- Impose an ordering on NFAInputs so they can be used in Maps
instance Ord NFAInput where
    Epsilon <= _           = True
    Symbol _ <= Epsilon    = False
    Symbol c1 <= Symbol c2 = c1 <= c2

data NFA = NFA
    {
        nodes :: G.Graph () NFAInput, -- In an NFA, we don't have any labels on the nodes
        start :: G.NodeIndex,
        acceptors :: S.Set G.NodeIndex
    } deriving Show


-- The empty expression
empty :: NFA
empty = NFA { nodes = g, start = 0, acceptors = S.singleton 1 }
            where g = G.buildGraph [G.Node (), G.Node ()] [(0, Epsilon, 1)]

-- The expression of a single symbol
singleton :: Char -> NFA
singleton c = NFA { nodes = g, start = 0, acceptors = S.singleton 1 }
            where g = G.buildGraph [G.Node (), G.Node ()] [(0, Symbol c, 1)]

-- Union of two expressions
--union :: NFA -> NFA
--union g1 g2