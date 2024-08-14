module AOC.Graph.Degeneracy where

import           AOC.Prelude
import           AOC.Graph.Base (Graph)
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import qualified Data.HashPSQ as Q

degeneracyOrdering :: (Ord a, Hashable a) => Graph a -> [a]
degeneracyOrdering g = go initialQueue where
    initialQueue = Q.fromList [ (v, Set.size nbors,()) | (v, nbors) <- Map.toList g]
    go queue = case Q.minView queue of
        Nothing -> []
        Just (v, _, _, queue')  -> v : go (foldl' decPriority queue' (g Map.! v)) where
            decPriority q u = snd $ Q.alter (\case
                                    Nothing -> ((), Nothing)
                                    Just (p, ()) -> ((), Just (p-1,()))
                                ) u q