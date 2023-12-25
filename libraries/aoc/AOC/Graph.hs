module AOC.Graph (module B, module S, module MM, module MC, perfectMatchings) where
import           AOC.Prelude
import qualified Data.HashSet as Set
import           AOC.List (minimumOn)
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
