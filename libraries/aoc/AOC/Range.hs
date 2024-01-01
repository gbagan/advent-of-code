module AOC.Range where

import AOC.Prelude hiding (show)
import Prelude (show)

data Range a = Range { _lower :: !a,  _upper :: !a } deriving (Eq, Ord)

instance Show a => Show (Range a) where
    show (Range a b) = "[" ++ show a ++ ".." ++ show b ++ "]"

member :: Ord a => a -> Range a -> Bool
member x (Range y z) = y <= x && x <= z

{-# INLINE member #-}

notMember :: Ord a => a -> Range a -> Bool
notMember x r = not (member x r)

{-# INLINE notMember #-}

length :: Num a => Range a -> a
length (Range lower upper) = upper + 1 - lower

{-# INLINE length #-}

intersection :: Ord a => Range a -> Range a -> Maybe (Range a)
intersection (Range begin1 end1) (Range begin2 end2)
    | end1 < begin2 || end2 < begin1 = Nothing
    | otherwise = Just $ Range (max begin1 begin2) (min end1 end2)

translate :: Num a => a -> Range a -> Range a
translate x (Range begin end) = Range (x + begin) (x + end)

{-# INLINE translate #-}

isSubsetOf :: Ord a => Range a -> Range a -> Bool
isSubsetOf (Range x1 y1) (Range x2 y2) = x2 <= x1 && y1 <= y2

{-# INLINE isSubsetOf #-}

overlaps :: Ord a => Range a -> Range a -> Bool
overlaps (Range x1 y1) (Range x2 y2) = max x1 x2 <= min y1 y2

{-# INLINE overlaps #-}

-- | two (discrete) ranges quasioverlap if their union is a range
isConnected :: Integral a => Range a -> Range a -> Bool
isConnected (Range x1 y1) (Range x2 y2) = max x1 x2 <= min y1 y2 + 1

{-# INLINE isConnected #-}

union :: Integral a => Range a -> Range a -> Maybe (Range a)
union itv1@(Range x1 y1) itv2@(Range x2 y2)
    | itv1 `isConnected` itv2 = Just $ Range (min x1 x2) (max y1 y2)
    | otherwise = Nothing

{-# INLINE union #-}

-- | return a set of disjoint intervals that contains the same points as the input
toDisjointUnion :: Integral a => [Range a] -> [Range a]
toDisjointUnion = go . sort where
    go [] = []
    go [x] = [x]
    go (itv1:itv2:itvs) = case itv1 `union` itv2 of
        Nothing -> itv1 : go (itv2 : itvs)
        Just itv' -> go (itv' : itvs)

{-# INLINE toDisjointUnion #-}