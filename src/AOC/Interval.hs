module AOC.Interval where

import AOC.Prelude

data Interval a = Interval { _start :: !a,  _end :: !a } deriving (Eq, Ord, Show)

member :: Ord a => a -> Interval a -> Bool
member x (Interval y z) = y <= x && x <= z

notMember :: Ord a => a -> Interval a -> Bool
notMember x = not . member x

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
isConnected :: Integral a => Interval a -> Interval a -> Bool
isConnected (Interval x1 y1) (Interval x2 y2) = max x1 x2 <= min y1 y2 + 1

-- | two (discrete) intervals quasioverlap if their union is an interval
quasiOverlap :: Interval Integer -> Interval Integer -> Bool
quasiOverlap (Interval x1 y1) (Interval x2 y2) = max x1 x2 <= min y1 y2 + 1

union' :: Integral a => Interval a -> Interval a -> Maybe (Interval a)
union' itv1@(Interval x1 y1) itv2@(Interval x2 y2)
    | itv1 `isConnected` itv2 = Just $ Interval (min x1 x2) (max y1 y2)
    | otherwise = Nothing

-- | return a set of disjoint intervals that contains the same points as the input
toDisjointUnion' :: Integral a => [Interval a] -> [Interval a]
toDisjointUnion' = go . sort where
    go [] = []
    go [x] = [x]
    go (itv1:itv2:itvs) = case union' itv1 itv2 of
        Nothing -> itv1 : go (itv2 : itvs)
        Just itv' -> go (itv' : itvs)