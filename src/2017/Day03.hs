-- https://adventofcode.com/2017/day/3
module Day03 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Parser (decimal)
import           Data.List ((!!))
import           AOC.V2 (V2, origin, west, east, south, north, manhattan, surrounding)
import qualified Data.HashMap.Strict as Map

spiral :: [V2 Int]
spiral = scanl' (+) origin $ go 1 where
    go n = replicate n east
        ++ replicate n north
        ++ replicate (n+1) west
        ++ replicate (n+1) south
        ++ go (n+2)

spiral2 :: [Int]
spiral2 = go Map.empty spiral where
    go _ [] = error "cannot happen since spiral is infinite"
    go prev (x:xs) = val : go prev' xs where
        val = max 1 . sum $ mapMaybe (prev Map.!?) (surrounding x)
        prev' = Map.insert x val prev

part1 :: Int -> Int
part1 n = manhattan origin $ spiral !! (n-1) 

part2 :: Int -> Maybe Int
part2 n = find (> n) spiral2

solve :: Text -> IO ()
solve = aoc decimal part1 part2