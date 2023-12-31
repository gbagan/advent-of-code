-- https://adventofcode.com/2016/day/19
module Day19 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Parser (decimal)

import           Data.Bits

-- https://en.wikipedia.org/wiki/Josephus_problem

part1 :: Int -> Int
part1 n = ((n `shiftL` 1) .|. 1) `clearBit` (finiteBitSize n - countLeadingZeros n)

part2 :: Int -> Int
part2 n = aux 1 where
    aux i | 3 * i >= n = n - i
          | otherwise  = aux (3 * i)

solve :: Text -> IO ()
solve = aoc decimal part1 part2


{-
part1 :: Int -> Int
part1 n = runST do
    vec <- V.generate n \i -> if i == n-1 then 0 else i + 1
    (+1) <$> run vec 0

run :: V.MVector s Int -> Int -> ST s Int
run vec current = do
    next <- V.read vec current
    if current == next then
        pure current
    else do
        next2  <- V.read vec next
        V.write vec current next2
        run vec next2
-}