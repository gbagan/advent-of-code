-- https://adventofcode.com/2016/day/13
module Day13 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Parser (decimal)
import           Data.Bits (popCount)
import           AOC.V2 (V2(..), adjacent)
import           AOC.Graph (bfs, distance)

isOpen :: Int -> V2 Int -> Bool
isOpen n (V2 y x) = x >=0 && y >=0 && even (popCount (x*x + 3*x + 2*x*y + y + y*y + n))

neighbors :: Int -> V2 Int -> [V2 Int]
neighbors n = filter (isOpen n) . adjacent

part1 :: Int -> Maybe Int
part1 n = distance (neighbors n) (== V2 39 31) (V2 1 1)

part2 :: Int -> Int
part2 n = length . takeWhile ((<=50) . fst) $ bfs (neighbors n) (V2 1 1)

solve :: Text -> IO ()
solve = aoc decimal part1 part2