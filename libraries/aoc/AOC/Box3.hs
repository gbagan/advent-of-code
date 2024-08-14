module AOC.Box3 where

import AOC.Prelude hiding (elem, transpose)
import AOC.V3 (V3(..))

data Box3 a = Box3
    { _min :: !(V3 a)
    , _max :: !(V3 a)
    }

elem :: Ord a => V3 a -> Box3 a -> Bool
elem v (Box3 min_ max_) = and (liftA2 (<=) min_ v) && and (liftA2 (<=) v max_) 

intersection :: Ord a => Box3 a -> Box3 a -> Maybe (Box3 a)
intersection b1@(Box3 min1 max1) b2@(Box3 min2 max2)
    | intersects b1 b2 = Just $! Box3 (liftA2 max min1 min2) (liftA2 min max1 max2)
    | otherwise = Nothing

intersects :: Ord a => Box3 a -> Box3 a -> Bool
intersects (Box3 min1 max1) (Box3 min2 max2) = and (liftA2 (<=) min2 max1) && and (liftA2 (<=) min1 max2)