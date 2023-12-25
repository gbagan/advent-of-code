module AOC.Graph.MinCut where

import           AOC.Prelude hiding (head, tail, last)
import           Data.List (head, tail)
import qualified Data.HashSet as Set
import qualified Data.HashMap.Strict as Map
import qualified Data.HashPSQ as Q
import           AOC.List (minimumOn)
import           AOC.Tuple (fst3, snd3, thd3, third3)
import           AOC.Graph (Graph, WeightedGraph, toWeightedGraph)

type CGraph a e w = HashMap a [(e, w, a)]

mergeVertices :: Hashable a => a -> a -> CGraph a e w -> CGraph a e w
mergeVertices u v graph = foldl' go graph' nborV 
    where
    go g (_, _, x) = Map.adjust (map (third3 \y -> if y == v then u else y)) x g
    graph' = Map.adjust (\nborU -> filter ((`notElem` [u, v]) . thd3) (nborU ++ nborV)) u
            $ Map.delete v graph
    nborV = graph Map.! v

{-# INLINE mergeVertices #-}

minimumCutStep :: (Ord a, Hashable a, Real w) => a -> CGraph a e w -> ([e], w, a, a)
minimumCutStep start graph = go (Q.singleton start 0 ()) [start] (Set.singleton start)
    where
    go queue found seen = case Q.minView queue of
        Nothing ->
            let last = head found
                prelast = head (tail found)
                lastNbor = graph Map.! last
            in (map fst3 lastNbor, sum (map snd3 lastNbor), last, prelast)
        Just (v, _, _, queue') -> go queue'' (v : found) (Set.insert v seen)
            where
            queue'' = foldl' 
                        (\q (_, w, u) -> 
                            if u `Set.member` seen
                                then q
                                else Q.insert
                                    u 
                                    (case Q.lookup u q of
                                        Nothing -> -w
                                        Just (p, _) -> p - w 
                                    )
                                    ()
                                    q
                        ) 
                        queue'
                        (graph Map.! v)

{-# INLINE minimumCutStep #-}

_minimumCut :: (Ord a, Hashable a, Real w) => CGraph a e w -> ([e], w)
_minimumCut g | Map.size g <= 1 = ([], 0)
              | null (g Map.! a) = ([], 0)
              | otherwise = minimumOn snd [cutset', (cutset, weight)]
            where
            a = head (Map.keys g)
            (cutset, weight, u, v) = minimumCutStep a g
            cutset' = _minimumCut (mergeVertices u v g) 

{-# INLINE _minimumCut #-}

minimumCut' :: (Ord a, Hashable a, Real w) => WeightedGraph a w -> ([(a, a)], w)
minimumCut' = _minimumCut . Map.mapWithKey \u nbor -> [((u, v), w, v) | (v, w) <- nbor]

{-# INLINE  minimumCut' #-}

minimumCut :: (Ord a, Hashable a) => Graph a -> [(a, a)]
minimumCut = fst . minimumCut' . toWeightedGraph

{-# INLINE  minimumCut #-}