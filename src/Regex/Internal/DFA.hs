module Regex.Internal.DFA where

import qualified Data.Vector as V
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad (sequence_, forM)
import qualified Control.Monad.Trans.State as ST
import Data.Maybe (mapMaybe, catMaybes, listToMaybe, fromJust)

import qualified Regex.Internal.NFA as N
import qualified Regex.Internal.Graph as G


type NFAIndex = Int -- Index of an NFA in the lookup
type NFAStateIndex = G.NodeIndex -- Index of a state in an NFA
type NFAPriority = Int -- Priority of an NFA

type NFALookup = V.Vector (NFAPriority, N.NFA)

data DFAInput = Symbol Char deriving (Eq, Show, Ord)
type DFAState = S.Set (NFAIndex, [NFAStateIndex])
type DFAAdjList = G.AdjList DFAState DFAInput -- In a DFA, nodes are labelled with NFA information

data DFA = DFA
    {
        graph :: DFAAdjList,
        start :: G.NodeIndex,
        acceptors :: S.Set G.NodeIndex
    }

convertInput :: N.NFAInput -> DFAInput
convertInput N.Epsilon = error "Invalid input to DFA"
convertInput (N.Symbol x) = Symbol x

-- Need to convert NFA list to a start state
-- Need to carry the start state information out of the conversion so the DFA can have that information
-- (although technically, the start state is always index 0 in the vector. Can use that)
-- Need to carry or calculate the set of accepting nodes out

createStartState :: [N.NFA] -> DFAState
createStartState = undefined

-- State monad, probably - accumulate the lookup for the DFA
convert :: [N.NFA] -> DFA
convert ns = snd $ eval
            where nfas = V.fromList $ zip [1..] ns :: NFALookup
                  startState = createStartState ns :: DFAState

convertCalc :: NFALookup -> DFAState -> ST.State (M.Map DFAState G.NodeIndex, DFAAdjList) ()
convertCalc nfas state = do
        let inputs = possibleInputs nfas state
            
        neighbourTransitions <- forM inputs $ \input -> do
                                    let newState = nextState nfas state input
                                    index <- addState newState
                                    return (convertInput input, [index])

        -- Update the pointers from this state to its neighbours
        (stateMap, states) <- ST.get
        let index = fromJust $ M.lookup state stateMap
            (node, _) = states V.! index
            newNode = (node, M.fromList neighbourTransitions)
            newStates = states V.// [(index, newNode)]

        ST.put (stateMap, newStates)


addState :: DFAState -> ST.State (M.Map DFAState G.NodeIndex, DFAAdjList) G.NodeIndex
addState state = do
        (stateMap, states) <- ST.get
        case M.lookup state stateMap of
            Just ix  -> return ix -- Already in the graph
            Nothing -> do
                    let newAdjList = V.snoc states (G.Node state, M.empty)
                        newIndex = V.length newAdjList - 1
                        newStateMap = M.insert state newIndex stateMap
                    ST.put $ (newStateMap, newAdjList)
                    return newIndex

-- Won't work properly - need to trim out empty lists using some Maybe magic
-- Leaving until I can actually test it
-- mapMaybe listToMaybe
nextState :: NFALookup -> DFAState -> N.NFAInput -> DFAState
nextState nfas state input = S.map (\(i, si) -> (i, nextNFAStates (snd $ nfas V.! i) input si)) state

nextNFAStates :: N.NFA -> N.NFAInput -> [NFAStateIndex] -> [NFAStateIndex]
nextNFAStates (N.NFA g _ _) i ss = concat $ catMaybes $ map (M.lookup i) $ map snd $ map (g V.!) ss

possibleInputs :: NFALookup -> DFAState -> [N.NFAInput]
possibleInputs nfas = concatMap (\(ni, ix) -> N.possibleInputs (snd $ nfas V.! ni) ix)