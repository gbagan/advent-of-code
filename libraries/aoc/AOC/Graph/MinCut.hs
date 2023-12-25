module AOC.Graph.MinCut where

import           AOC.Prelude hiding (head, tail, last)
import           Data.List (head, tail)
import qualified Data.HashSet as Set
import qualified Data.HashMap.Strict as Map
import qualified Data.HashPSQ as Q
import           AOC.Graph (Graph)

type CGraph a e = HashMap a [(e, a)]

removeVertex :: Hashable a => a -> CGraph a e -> CGraph a e
removeVertex v graph = case graph Map.!? v of
    Nothing -> graph
    Just nbor -> foldl'
        (\g (_, u) -> Map.adjust (filter ((/=v) . snd)) u g)
        (Map.delete v graph)
        nbor

mergeVertices :: Hashable a => a -> a-> CGraph a e -> CGraph a e
mergeVertices u v graph = foldl' go graph' nborV 
    where
    go g (_, w) = Map.adjust (map (\(e, w') -> (e, if w' == v then u else w'))) w g
    graph' = Map.adjust (\nborU -> filter ((`notElem` [u, v]) . snd) (nborU ++ nborV)) u
            $ Map.delete v graph
    nborV = graph Map.! v

minimumCutPhase :: (Ord a, Hashable a) => a -> CGraph a e -> ([e], a, a)
minimumCutPhase start graph = go 
                            (Q.singleton start (-1::Int) ())
                            -- (Q.fromList [(v,-1::Int,()) | (_, v) <- graph Map.! start]) 
                            []
                            graph
    where
    go queue found g = case Q.minView queue of
        Nothing -> 
            let last = head found
                prelast = head (tail found)
                lastNbor = map fst (graph Map.! last)
            in (lastNbor, last, prelast)
        Just (v, _, _, queue') ->
            let nbor = g Map.! v
                g' = removeVertex v g
                queue'' = foldl' 
                            (\q (_, u) -> 
                                Q.insert
                                    u 
                                    (case Q.lookup u q of
                                        Nothing -> (-1)
                                        Just (p, _) -> p - 1 
                                    )
                                    ()
                                    q
                            ) 
                            queue'
                            nbor 
            in
            go queue'' (v : found) g'

_minimumCut :: (Ord a, Hashable a) => CGraph a e -> [e]
_minimumCut g | Map.size g <= 1 = []
              | Map.size g == 2 = map fst $ head (Map.elems g)
              | otherwise =
                    let a = head (Map.keys g)
                        (cutset, u, v) = minimumCutPhase a g
                        cutset' = _minimumCut (mergeVertices u v g)
                    in
                    if length cutset > length cutset' then cutset' else cutset

minimumCut :: (Ord a, Hashable a) => Graph a -> [(a, a)]
minimumCut = _minimumCut . Map.mapWithKey \u nbor -> [((u, v), v) | v <- Set.toList nbor]