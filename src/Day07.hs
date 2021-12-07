module Day07 (solve) where
import Data.List (sort, genericLength)
import Data.List.Split (splitOn)
import Text.Read (readMaybe)
import Util (average)

solve1 :: [Int] -> Int
solve1 xs = sum [abs (x - m) | x <- xs] where
        m = xs !! (length xs `div` 2)

solve2 :: [Int] -> Int
solve2 xs = sum [bin . abs $ x - m | x <- xs] where
        m = floor (average xs)
        bin n = n * (n + 1) `div` 2

solve :: String -> Maybe (Int, Int)
solve s = do
    xs <- sort <$> traverse readMaybe (splitOn "," s)
    pure (solve1 xs, solve2 xs)