module AOC.Graph.Clique (maximalCliques, maximalCliques2) where

import           AOC.Prelude
import           AOC.Graph.Base (Graph)
import           AOC.Graph.Degeneracy (degeneracyOrdering)
import           AOC.List (headMaybe)
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set

maximalCliques :: Hashable a => Graph a -> [[a]]
maximalCliques g = bronKerbosch g Set.empty (Map.keysSet g) Set.empty
{-# INLINE maximalCliques #-}

maximalCliques2 :: (Ord a, Hashable a) => Graph a -> [[a]]
maximalCliques2 g = go g Set.empty (Map.keysSet g) Set.empty (degeneracyOrdering g)
{-# INLINE maximalCliques2 #-}

bronKerbosch :: Hashable a => Graph a -> HashSet a -> HashSet a -> HashSet a -> [[a]]
bronKerbosch !g !r !p !x =
    let px = p <> x in
    case headMaybe (Set.toList px) of
        Nothing -> [Set.toList r]
        Just u -> go g r p x (Set.toList (p `Set.difference` (g Map.! u)))
{-# INLINE bronKerbosch #-}

go :: Hashable a => Graph a -> HashSet a -> HashSet a -> HashSet a -> [a] -> [[a]]
go _ _ _ _ [] = []
go !g !r !p !x (v:nbors) = 
    let nv = g Map.! v in
    bronKerbosch g (Set.insert v r) (p `Set.intersection` nv) (x `Set.intersection` nv) 
    ++ go g r (Set.delete v p) (Set.insert v x) nbors
{-# INLINE go #-}