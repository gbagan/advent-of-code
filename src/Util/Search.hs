module Util.Search where

import           RIO
import           RIO.List (find)
import           RIO.Seq (Seq(..), (><))
import qualified RIO.Seq as Seq
import qualified RIO.Set as Set
import qualified RIO.HashSet as HSet

bfs :: Hashable a => (a -> [a]) -> a -> [(Int, a)]
bfs nborFunc start = go HSet.empty (Seq.singleton (0, start)) where
    go _ Empty = []
    go visited ((d, v) :<| queue)
        | v `HSet.member` visited = go visited queue
        | otherwise = (d, v) : go
                        (HSet.insert v visited)
                        (queue >< Seq.fromList [(d+1, u) | u <- nborFunc v])

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

dijkstra :: (Ord v, Real w) => (v -> [(v, w)]) -> v -> v -> Maybe w
dijkstra nbors source target = go Set.empty (Set.singleton (0, source)) where
    go visited queue = case Set.minView queue of
        Nothing -> Nothing
        Just ((cost, v), queue')
            | v == target          -> Just cost
            | Set.member v visited -> go visited queue'
            | otherwise            -> go
                                        (Set.insert v visited)
                                        (foldl' (flip Set.insert) queue' [(w+cost, u) | (u, w) <- nbors v])