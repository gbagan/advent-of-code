module AOC.Interval where

import AOC.Prelude

data Interval a = Interval { _begin :: !a,  _end :: !a } deriving (Eq, Ord, Show)

intersection :: Ord a => Interval a -> Interval a -> Maybe (Interval a)
intersection (Interval begin1 end1) (Interval begin2 end2)
    | end1 < begin2 || end2 < begin1 = Nothing
    | otherwise = Just $ Interval (max begin1 begin2) (min end1 end2)

translate :: Num a => a -> Interval a -> Interval a
translate x (Interval begin end) = Interval (x + begin) (x + end)

isSubsetOf :: Ord a => Interval a -> Interval a -> Bool
isSubsetOf (Interval x1 y1) (Interval x2 y2) = x2 <= x1 && y1 <= y2

overlaps :: Ord a => Interval a -> Interval a -> Bool
overlaps (Interval x1 y1) (Interval x2 y2) = max x1 x2 <= min y1 y2

-- | two (discrete) intervals quasioverlap if their union is an interval
quasiOverlaps :: Integral a => Interval a -> Interval a -> Bool
quasiOverlaps (Interval x1 y1) (Interval x2 y2) = max x1 x2 <= min y1 y2 + 1

-- | two (discrete) intervals quasioverlap if their union is an interval
quasiOverlap :: Interval Integer -> Interval Integer -> Bool
quasiOverlap (Interval x1 y1) (Interval x2 y2) = max x1 x2 <= min y1 y2 + 1

union' :: Integral a => Interval a -> Interval a -> Interval a
union' itv1@(Interval x1 y1) itv2@(Interval x2 y2)
    | itv1 `quasiOverlaps` itv2 = Interval (min x1 x2) (max y1 y2)
    | otherwise = error "union: not an interval"