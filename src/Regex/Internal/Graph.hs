module Regex.Internal.Graph where

import qualified Data.Map as M
import qualified Data.Vector as V

data Node a = Node a deriving Show
type NodeIndex = Int
type Edge a = (NodeIndex, a, NodeIndex)

srcNode :: Edge a -> NodeIndex
srcNode (s,_,_) = s

arcInfo :: Edge a -> (a, NodeIndex)
arcInfo (_,x,d) = (x,d)

data Graph a b = Graph
    {
        adjList :: V.Vector (Node a, M.Map b NodeIndex)
    } deriving Show


buildGraph :: Ord b => [Node a] -> [Edge b] -> Graph a b
buildGraph nodes edges = Graph { adjList = V.fromList elements }
        where elements = zip nodes (map getAdjacent [0..])

              getAdjacent i = M.fromList $ map arcInfo $ filter ((== i) . srcNode) edges