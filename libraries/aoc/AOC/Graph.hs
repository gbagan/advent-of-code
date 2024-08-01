module AOC.Graph (module B, module S, module MM, module MC, perfectMatchings, lexicographicTopologicalOrdering, dagLongestPath) where
import           AOC.Prelude
import qualified Data.HashSet as Set
import           AOC.List (minimumOn, maximumDef)
import qualified AOC.HashMap as Map
import qualified Data.Set as OSet
import           AOC.Graph.Base as B
import           AOC.Graph.Search as S
import           AOC.Graph.MaximumMatching as MM
import           AOC.Graph.MinCut as MC

perfectMatchings :: (Eq a, Hashable b) => [(a, [b])] ->  [[(a, b)]]
perfectMatchings = go . map (second Set.fromList) where
    go [] = [[]]
    go g = do
        let (v, nbors) = minimumOn (Set.size . snd) g
        u <- Set.toList nbors
        ((v, u) :) <$> go [ (w, Set.delete u nbors')  | (w, nbors') <- g, v /= w]

{-# INLINE perfectMatchings #-}

lexicographicTopologicalOrdering :: (Hashable a, Ord a) => [(a, a)] -> [a]
lexicographicTopologicalOrdering edges = go initialQueue incomingDegree where
    outgoing = foldl' (\deg (u, v) -> Map.insertWith (++) u [v] deg) Map.empty edges
    incomingDegree = foldl' (\deg (_, v) -> Map.insertWith (+) v (1::Int) deg) Map.empty edges
    initialQueue = OSet.fromList [u | (u, _) <- edges, u `Map.notMember` incomingDegree]
    go queue incDeg = case OSet.minView queue of
        Nothing -> []
        Just (v, queue') -> v : go queue'' incDeg' where
            (incDeg', queue'') = foldl' go' (incDeg, queue') (outgoing Map.!? v ?: [])
    go' (incDeg, queue) v =
        let deg = incDeg Map.! v - 1 in
        if deg == 0
            then (incDeg, OSet.insert v queue)
            else (Map.insert v deg incDeg, queue)

{-# INLINE lexicographicTopologicalOrdering #-}

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