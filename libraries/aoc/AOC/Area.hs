module AOC.Area where

import AOC.Prelude hiding (elem, transpose)
import AOC.V2 (V2(..))

data Area a = Area
    { _xmin :: !a
    , _ymin :: !a
    , _xmax :: !a
    , _ymax :: !a
    }

elem :: Ord a => V2 a -> Area a -> Bool
elem (V2 x y) (Area xmin ymin xmax ymax) = xmin <= x && x <= xmax && ymin <= y && y <= ymax

expand :: Num a => a -> Area a -> Area a
expand n (Area xmin ymin xmax ymax) = Area (xmin - n) (ymin + n) (xmax - n) (ymax + n)

transpose :: Area a -> Area a
transpose (Area xmin ymin xmax ymax) = Area ymin xmin ymax xmax

intersect :: Ord a => Area a -> Area a -> Maybe (Area a)
intersect (Area xmin1 ymin1 xmax1 ymax1) (Area xmin2 ymin2 xmax2 ymax2)
    | xmax1 < xmin2 || xmax2 < xmin1 || ymax1 < ymin2 || ymax2 < ymin1 = Nothing
    | otherwise = Just $! Area
                            (max xmin1 xmin2)
                            (max ymin1 ymin2)
                            (min xmax1 xmax2)
                            (min ymin1 ymax2)
