module AOC.Search where

import           Relude
import           Data.Sequence (Seq(..), (><))
import qualified Data.Sequence as Seq
import qualified Data.HashSet as HSet
import qualified Data.Heap as H

bfs :: Hashable a => (a -> [a]) -> a -> [(Int, a)]
bfs nborFunc start = go HSet.empty (Seq.singleton (0, start)) where
    go _ Empty = []
    go visited ((d, v) :<| queue)
        | v `HSet.member` visited = go visited queue
        | otherwise = (d, v) : go
                        (HSet.insert v visited)
                        (queue >< Seq.fromList [(d+1, u) | u <- nborFunc v])

reachableFrom :: Hashable a => (a -> [a]) -> a -> HashSet a
reachableFrom nborFunc start = go HSet.empty [start] where
    go visited [] = visited
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

dijkstra :: (Hashable v, Real w) => (v -> [(v, w)]) -> (v -> Bool) -> v -> Maybe w
dijkstra nborFunc targetFunc source = dijkstra' nborFunc targetFunc [source]
    
dijkstra' :: (Hashable v, Real w) => (v -> [(v, w)]) -> (v -> Bool) -> [v] -> Maybe w
dijkstra' nborFunc targetFunc sources = go HSet.empty (H.fromList @H.FstMinPolicy . map (0,) $ sources) where
    go visited queue = case H.view queue of
        Nothing -> Nothing
        Just ((cost, v), queue')
            | targetFunc v         -> Just cost
            | HSet.member v visited -> go visited queue'
            | otherwise            -> go
                                        (HSet.insert v visited)
                                        (foldl' (flip H.insert) queue' [(w+cost, u) | (u, w) <- nborFunc v])