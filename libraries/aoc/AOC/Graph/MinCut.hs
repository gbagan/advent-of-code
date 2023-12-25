module AOC.Graph.MinCut where

import           AOC.Prelude hiding (head, tail, last)
import           Data.List (head, tail)
import qualified Data.HashSet as Set
import qualified Data.HashMap.Strict as Map
import qualified Data.HashPSQ as Q
import           AOC.Graph (Graph)

removeVertex :: Hashable a => a -> HashMap a [(a, a)] -> HashMap a [(a, a)]
removeVertex v graph = case graph Map.!? v of
    Nothing -> graph
    Just nbor -> foldl'
        (\g (_, u) -> Map.adjust (filter ((/=v) . snd)) u g)
        (Map.delete v graph)
        nbor

mergeVertices :: Hashable a => a -> a-> HashMap a [(a, a)] -> HashMap a [(a, a)]
mergeVertices u v graph = graph -- todo


minimumCutPhase :: (Ord a, Hashable a) => HashMap a [(a, a)] -> ([(a, a)], a, a)
minimumCutPhase graph = go 
                            (Q.fromList [ (v,-length nbor,()) | (v, nbor) <- Map.toList graph]) 
                            []
                            graph
    where
    go queue found g = case Q.minView queue of
        Nothing -> 
            let last = head found
                prelast = head (tail found)
                lastNbor = g Map.! last
            in (lastNbor, last, prelast)
        Just (v, _, _, queue') ->
            let nbor = g Map.! v
                g' = removeVertex v g
                queue'' = foldl' 
                            (\q (_, u) -> Q.insert u (-length (g' Map.! u)) () q)
                            queue'
                            nbor 
            in
            go queue'' (v : found) g'