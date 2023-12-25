module AOC.Graph.MinCut where

import           AOC.Prelude hiding (head, tail, last)
import           Data.List (head, tail)
import qualified Data.HashSet as Set
import qualified Data.HashMap.Strict as Map
import qualified Data.HashPSQ as Q
import           AOC.Graph (Graph)

type CGraph a e = HashMap a [(e, a)]

mergeVertices :: Hashable a => a -> a -> CGraph a e -> CGraph a e
mergeVertices u v graph = foldl' go graph' nborV 
    where
    go g (_, w) = Map.adjust (map (\(e, w') -> (e, if w' == v then u else w'))) w g
    graph' = Map.adjust (\nborU -> filter ((`notElem` [u, v]) . snd) (nborU ++ nborV)) u
            $ Map.delete v graph
    nborV = graph Map.! v

{-# INLINE mergeVertices #-}

minimumCutStep :: (Ord a, Hashable a) => a -> CGraph a e -> ([e], a, a)
minimumCutStep start graph = go (Q.singleton start (-1::Int) ()) [start] (Set.singleton start)
    where
    go queue found seen = case Q.minView queue of
        Nothing ->
            let last = head found
                prelast = head (tail found)
                lastNbor = map fst (graph Map.! last)
            in (lastNbor, last, prelast)
        Just (v, _, _, queue') ->
            let queue'' = foldl' 
                            (\q (_, u) -> 
                                if u `Set.member` seen
                                    then q
                                    else Q.insert
                                        u 
                                        (case Q.lookup u q of
                                            Nothing -> (-1)
                                            Just (p, _) -> p - 1 
                                        )
                                        ()
                                        q
                            ) 
                            queue'
                            (graph Map.! v)
            in
            go queue'' (v : found) (Set.insert v seen)

{-# INLINE minimumCutStep #-}

_minimumCut :: (Ord a, Hashable a) => CGraph a e -> [e]
_minimumCut g | Map.size g <= 1 = []
              | null (g Map.! a) = []
              | otherwise = if length cutset > length cutset' then cutset' else cutset 
            where
            a = head (Map.keys g)
            (cutset, u, v) = minimumCutStep a g
            cutset' = _minimumCut (mergeVertices u v g) 

{-# INLINE _minimumCut #-}

minimumCut :: (Ord a, Hashable a) => Graph a -> [(a, a)]
minimumCut = _minimumCut . Map.mapWithKey \u nbor -> [((u, v), v) | v <- Set.toList nbor]

{-# INLINE  minimumCut #-}