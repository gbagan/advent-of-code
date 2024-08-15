module AOC.Util where
import           AOC.Prelude
import           Data.Char (digitToInt, isDigit)
import           Data.Maybe (fromJust)
import           AOC.List (count, findDuplicate')
import qualified Data.HashMap.Strict as HMap
import qualified Data.HashSet as HSet
import           AOC.V2 (V2(..))

times :: Int -> (a -> a) -> a -> a
times n f x
    | n <= 0 = x
    | otherwise = times (n-1) f $! f x
{-# INLINE times #-}

timesM :: Monad m => Int -> (a -> m a) -> a -> m a
timesM n f x
    | n <= 0    = pure x
    | otherwise = do
        x' <- f x
        timesM (n-1) f x'
{-# INLINE timesM #-}

timesM_ :: Monad m => Int -> m () -> m ()
timesM_ n f
    | n <= 0    = pure ()
    | otherwise = f *> timesM_ (n-1) f
{-# INLINE timesM_ #-}

-- similar to the functions times but assumes that n is huge and there is a cycle in the iteration
manyTimes :: Hashable a => Int -> (a -> a) -> a -> a
manyTimes n step x = times (remaining `rem` period) step y where
    (i, j, y) = fromJust $ findDuplicate' $ iterate' step x
    period = j - i
    remaining = n - j
{-# INLINE manyTimes #-}

majority :: (a -> Bool) -> [a] -> Bool
majority f l = 2 * count f l >= length l

clamp :: Ord a => (a, a) -> a -> a
clamp (l, u) = max l . min u
{-# INLINE clamp #-}

listTo2dMap :: [[a]] -> HashMap (V2 Int) a
listTo2dMap l =
    HMap.fromList
        [(V2 i j, v)
        | (i, row) <- zip [0..] l
        , (j, v) <- zip [0..] row
        ]

listTo2dSet :: [[Bool]] -> HashSet (V2 Int)
listTo2dSet l =
    HSet.fromList
        [ V2 i j
        | (i, row) <- zip [0..] l
        , (j, True) <- zip [0..] row
        ]

binToInt :: [Bool] -> Int
binToInt = foldl' (\acc x -> acc * 2 + fromEnum x) 0

hexToInt :: String -> Int
hexToInt = foldl' (\acc x -> acc * 16 + hexDigitToInt x) 0
   where hexDigitToInt x
          | isDigit x = digitToInt x
          | otherwise = ord x - ord 'a' + 10

maxBinSearch :: Integral a => (a -> Bool) -> a -> a -> a
maxBinSearch predicate a b
    | a + 1 == b    = a
    | predicate mid = maxBinSearch predicate mid b
    | otherwise     = maxBinSearch predicate a mid
    where
    mid = a + (b - a) `div` 2
{-# INLINE maxBinSearch #-}

maxBinSearch' :: Integral a => (a -> Bool) -> a
maxBinSearch' predicate = go 1 where
    go n | predicate n  = go (n*2)
         | otherwise = maxBinSearch predicate 1 n  
{-# INLINE maxBinSearch' #-}

minBinSearch :: (Int -> Maybe a) -> Int -> Maybe (Int, a) -> a
minBinSearch _ low (Just (high, value)) | low + 1 == high = value
minBinSearch f low mbHigh =
    case f i of
        Nothing -> minBinSearch f i mbHigh
        Just a  -> minBinSearch f low (Just (i, a))
    where
    i = case mbHigh of
        Nothing        -> low*2+1
        Just (high, _) -> (low+1 + high) `quot` 2
{-# INLINE minBinSearch #-}

-- return the double of the polygon area
shoelaceFormula :: Num a => [V2 a] -> a
shoelaceFormula points = abs . sum $ zipWith go points (drop 1 points ++ points)
    where go (V2 x y) (V2 x' y') = x * y' - x' * y