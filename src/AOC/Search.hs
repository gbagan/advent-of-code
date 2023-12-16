module AOC.Search where

import           Relude
import           Data.Sequence (Seq(..), (><))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.HashSet as HSet

bfs :: Hashable a => (a -> [a]) -> a -> [(Int, a)]
bfs nborFunc start = go HSet.empty (Seq.singleton (0, start)) where
    go _ Empty = []
    go visited ((d, v) :<| queue)
        | v `HSet.member` visited = go visited queue
        | otherwise = (d, v) : go
                        (HSet.insert v visited)
                        (queue >< Seq.fromList [(d+1, u) | u <- nborFunc v])

reachableFrom :: Hashable a => (a -> [a]) -> a -> [a]
reachableFrom nborFunc start = go HSet.empty [start] where
    go visited [] = HSet.toList visited
    go visited (v : stack)
        | v `HSet.member` visited = go visited stack
        | otherwise = go (HSet.insert v visited) (nborFunc v ++ stack)

distance :: Hashable a => (a -> [a]) -> (a -> Bool) -> a -> Maybe Int
distance nborFunc destFunc start =
    fst <$> find (destFunc . snd) (bfs nborFunc start)

dfsM :: (Hashable a, Monad m) => (a -> m [a]) -> a -> m ()
dfsM nborFunc start = go HSet.empty [start] where
    go _ [] = pure ()
    go visited (v:queue)
        | v `HSet.member` visited = go visited queue
        | otherwise = do
            nbors <- nborFunc v
            go (HSet.insert v visited) (nbors ++ queue)

dijkstra :: (Ord v, Real w) => (v -> [(v, w)]) -> (v -> Bool) -> v -> Maybe w
dijkstra nborFunc targetFunc source = go Set.empty (Set.singleton (0, source)) where
    go visited queue = case Set.minView queue of
        Nothing -> Nothing
        Just ((cost, v), queue')
            | targetFunc v         -> Just cost
            | Set.member v visited -> go visited queue'
            | otherwise            -> go
                                        (Set.insert v visited)
                                        (foldl' (flip Set.insert) queue' [(w+cost, u) | (u, w) <- nborFunc v])