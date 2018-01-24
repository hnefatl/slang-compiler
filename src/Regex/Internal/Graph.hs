{-# LANGUAGE ScopedTypeVariables #-}

module Regex.Internal.Graph where

import qualified Data.Map as M
import qualified Data.Vector as V
import Data.List (foldl')

data Node a = Node a deriving (Show, Eq)
type NodeIndex = Int
type Edge a = (NodeIndex, a, NodeIndex)

srcNode :: Edge a -> NodeIndex
srcNode (s,_,_) = s

arcInfo :: Edge a -> (a, NodeIndex)
arcInfo (_,x,d) = (x,d)

type AdjList a b = V.Vector (Node a, M.Map b [NodeIndex])

singleton :: a -> AdjList a b
singleton x = V.fromList [(Node x, M.empty)]

-- Construct a graph from a given list of nodes and list of edges.
buildGraph :: forall a b. Ord b => [Node a] -> [Edge b] -> AdjList a b
buildGraph nodes edges = V.fromList elements
        where elements :: [(Node a, M.Map b [NodeIndex])]
              elements = zip nodes (map getAdjacent [0..])

              getAdjacent :: NodeIndex -> M.Map b [NodeIndex]
              --getAdjacent i = M.fromList $ map ($ groupBy (\a b -> fst a == fst b) $ map arcInfo $ filter ((== i) . srcNode) edges
              getAdjacent i = foldl' inserter M.empty $ map arcInfo $ filter ((== i) . srcNode) edges

              inserter :: M.Map b [NodeIndex] -> (b, NodeIndex) -> M.Map b [NodeIndex]
              inserter m (k, v) = M.insertWith (++) k [v] m

insertEdge :: Ord b => AdjList a b -> Edge b -> AdjList a b
insertEdge list (s,x,d) = vectorUpdate (\(n, neighbours) -> (n, M.insertWith (++) x [d] neighbours)) s list

insertEdges :: Ord b => AdjList a b -> [Edge b] -> AdjList a b
insertEdges = foldl' insertEdge

-- Helper function - update a value at a given index in a vector
vectorUpdate :: (a -> a) -> Int -> V.Vector a -> V.Vector a
vectorUpdate f i v = v V.// [(i, f $ v V.! i)]