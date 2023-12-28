-- https://adventofcode.com/2015/day/20
module Day20 (solve) where
import           AOC.Prelude hiding (head)
import           Data.List (head)
import           AOC (aoc)
import           AOC.Parser (Parser, decimal)

primeDecomposition :: Int -> [(Int, Int)]
primeDecomposition = map (\n -> (head n, length n)) . group . aux 2 where
    aux _ 1                  = []
    aux p n | n `rem` p == 0 = p : aux p (n `quot` p)
            | p*p > n        = [n]
            | otherwise      = aux (p+1) n

divisorSigma1 :: Int -> Int
divisorSigma1 n = product [(p ^ (a+1) - 1) `div` (p - 1) | (p, a) <- primeDecomposition n]

part1 :: Int -> Maybe Int
part1 n = find (\i -> 10 * divisorSigma1 i >= n) [1..]

part2Number :: Int -> Int
part2Number n = sum [q | i <- [1..50], let (q, r) = n `quotRem` i, r == 0]

part2 :: Int -> Maybe Int
part2 n = find (\i -> 11 * part2Number i >= n) [1..]

solve :: Text -> IO ()
solve = aoc decimal part1 part2
