module AOC.Graph.Base where

import           AOC.Prelude
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set

type Graph a = HashMap a (HashSet a)
type WeightedGraph a w = HashMap a [(a, w)]

toWeightedGraph :: Graph a -> WeightedGraph a Int
toWeightedGraph = Map.map (map (,1) . Set.toList)

addEdge :: Hashable a => a -> a -> Graph a -> Graph a
addEdge u v = Map.alter (addToNbors u) v . Map.alter (addToNbors v) u where
    addToNbors w Nothing = Just $ Set.singleton w
    addToNbors w (Just nbors) = Just $ Set.insert w nbors 

removeEdge :: Hashable a => a -> a -> Graph a -> Graph a
removeEdge u v = Map.adjust (Set.delete v) u . Map.adjust (Set.delete u) v

removeVertex :: Hashable a => a -> Graph a -> Graph a
removeVertex v graph = case graph Map.!? v of
    Nothing -> graph
    Just nbor -> foldl'
                    (flip (Map.adjust (Set.delete v)))
                    (Map.delete v graph)
                    (Set.toList nbor)

fromEdgePredicate :: Hashable a => [a] -> (a -> a -> Bool) -> Graph a
fromEdgePredicate vertices p = Map.fromList [(v, nbors v) | v <- vertices] where
    nbors v = Set.fromList [u | u <- vertices, u /= v && p v u]