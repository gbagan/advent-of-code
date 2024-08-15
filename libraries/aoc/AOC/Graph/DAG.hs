module AOC.Graph.DAG where

import AOC.Prelude hiding (reverse)
import           AOC.Graph.Base (Graph, reverse)
import qualified AOC.HashMap as Map
import qualified AOC.HashSet as Set
import qualified Data.Set as OSet 

lexicographicTopologicalOrdering :: (Hashable a, Ord a) => Graph a -> [a]
lexicographicTopologicalOrdering graph = go initialQueue incomingDegree where
    -- outgoing = foldl' (\deg (u, v) -> Map.insertWith (++) u [v] deg) Map.empty edges
    incomingDegree = Map.map Set.size (reverse graph) 
    initialQueue = OSet.fromList [u | (u, 0) <- Map.toList incomingDegree]
    go queue incDeg = case OSet.minView queue of
        Nothing -> []
        Just (v, queue') -> v : go queue'' incDeg' where
            (incDeg', queue'') = foldl' go' (incDeg, queue') (graph Map.! v)
    go' (incDeg, queue) v =
        let deg = incDeg Map.! v - 1 in
        if deg == 0
            then (incDeg, OSet.insert v queue)
            else (Map.insert v deg incDeg, queue)

{-# INLINE lexicographicTopologicalOrdering #-}

{-
dagLongestPath :: (Hashable a, Ord a) => [(a, a, Int)] -> Int
dagLongestPath wedges = maximumDef 0 (map snd (Map.toList pathWeights)) where
    edges = [(u, v) | (u, v, _) <- wedges]
    ordering = lexicographicTopologicalOrdering edges
    incoming = foldl' (\deg (u, v, w) -> Map.insertWith (++) v [(u, w)] deg) Map.empty wedges
    pathWeights = foldl' go Map.empty ordering
    go weights v = Map.insert
                    v 
                    (maximumDef 0 [w + weights Map.! u | (u, w) <- Map.lookupDefault [] v incoming])
                    weights

{-# INLINE dagLongestPath #-}
-}