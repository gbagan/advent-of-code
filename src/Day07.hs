module Day07 (solve) where
import Data.List (sort, genericLength)
import Data.List.Split (splitOn)
import Text.Read (readMaybe)
import Util (average)

solve1 :: [Int] -> Int
solve1 xs =
    let m = xs !! (length xs `div` 2)
    in sum [abs (x - m) | x <- xs]

solve2 :: [Int] -> Int
solve2 xs = min t1 t2 where
        m = average xs
        m1 = minimum . filter ((>=m) . realToFrac) $ xs
        m2 = maximum . filter ((<=m) . realToFrac) $ xs
        t1 = sum [bin . abs $ x - m1 | x <- xs]
        t2 = sum [bin . abs $ x - m2 | x <- xs]
        bin n = n * (n + 1) `div` 2

solve :: String -> Maybe (Int, Int)
solve s = do
    xs <- sort <$> traverse readMaybe (splitOn "," s)
    pure (solve1 xs, solve2 xs)