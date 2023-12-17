module AOC.Search where

import           AOC.Prelude hiding (init)
import           Data.Sequence (Seq(..), (><))
import qualified Data.Sequence as Seq
import qualified Data.HashSet as Set
-- import qualified Data.HashMap.Strict as Map
import qualified Data.HashPSQ as Q

bfs :: Hashable a => (a -> [a]) -> a -> [(Int, a)]
bfs nborFunc start = go Set.empty (Seq.singleton (0, start)) where
    go _ Empty = []
    go visited ((d, v) :<| queue)
        | v `Set.member` visited = go visited queue
        | otherwise = (d, v) : go
                        (Set.insert v visited)
                        (queue >< Seq.fromList [(d+1, u) | u <- nborFunc v])

reachableFrom :: Hashable a => (a -> [a]) -> a -> HashSet a
reachableFrom nborFunc start = go Set.empty [start] where
    go visited [] = visited
    go visited (v : stack)
        | v `Set.member` visited = go visited stack
        | otherwise = go (Set.insert v visited) (nborFunc v ++ stack)

distance :: Hashable a => (a -> [a]) -> (a -> Bool) -> a -> Maybe Int
distance nborFunc destFunc start =
    fst <$> find (destFunc . snd) (bfs nborFunc start)

dfsM :: (Hashable a, Monad m) => (a -> m [a]) -> a -> m ()
dfsM nborFunc start = go Set.empty [start] where
    go _ [] = pure ()
    go visited (v:queue)
        | v `Set.member` visited = go visited queue
        | otherwise = do
            nbors <- nborFunc v
            go (Set.insert v visited) (nbors ++ queue)

dijkstra :: (Hashable v, Ord v, Real w) => (v -> [(v, w)]) -> (v -> Bool) -> v -> Maybe w
dijkstra nborFunc targetFunc source = dijkstra' nborFunc targetFunc [source]

{-# SPECIALISE dijkstra :: (Hashable v, Ord v) => (v -> [(v, Int)]) -> (v -> Bool) -> v -> Maybe Int #-}

dijkstra' :: (Hashable v, Ord v, Real w) => (v -> [(v, w)]) -> (v -> Bool) -> [v] -> Maybe w
dijkstra' nbors targetFunc sources = go Set.empty initialQueue where
    initialQueue = Q.fromList [(s,0,()) | s <- sources]
    go !visited !queue = case Q.minView queue of
        Nothing -> Nothing
        Just (v, cost, _, queue')
            | targetFunc v -> Just cost
            | otherwise -> go
                            (Set.insert v visited)
                            (foldl' insert queue'
                                [(v', cost+w') | (v', w') <- nbors v, not (v' `Set.member` visited)]
                            )
    insert queue (u, w) = case Q.lookup u queue of
        Just (w', _) | w' < w -> queue
        _ -> Q.insert u w () queue

{-# SPECIALISE dijkstra' :: (Hashable v, Ord v) => (v -> [(v, Int)]) -> (v -> Bool) -> [v] -> Maybe Int #-}
