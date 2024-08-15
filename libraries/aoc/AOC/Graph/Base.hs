module AOC.Graph.Base where

import           AOC.Prelude hiding (reverse)
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set

type Graph a = HashMap a (HashSet a)
type WeightedGraph a w = HashMap a [(a, w)]

toWeightedGraph :: Graph a -> WeightedGraph a Int
toWeightedGraph = Map.map (map (,1) . Set.toList)

addVertex :: Hashable a => a -> Graph a -> Graph a
addVertex v g | v `Map.member` g = g
              | otherwise = Map.insert v Set.empty g

addEdge :: Hashable a => a -> a -> Graph a -> Graph a
addEdge u v = addVertex v . Map.alter (addToNbors v) u where
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

fromEdges :: Hashable a => [(a, a)] -> Graph a
fromEdges = foldl' (\g (u, v) -> addEdge u v g) Map.empty

fromEdgePredicate :: Hashable a => [a] -> (a -> a -> Bool) -> Graph a
fromEdgePredicate vertices p = Map.fromList [(v, nbors v) | v <- vertices] where
    nbors v = Set.fromList [u | u <- vertices, u /= v && p v u]

edgeList :: Graph a -> [(a, a)]
edgeList = concatMap (\(v, nbors) -> fmap (v,) (Set.toList nbors)) . Map.toList

reverse :: Hashable a => Graph a -> Graph a
reverse = foldl' (\g (u, v) -> addEdge v u g) Map.empty . edgeList

symmetrize :: Hashable a => Graph a -> Graph a
symmetrize g = Map.unionWith Set.union g (reverse g)