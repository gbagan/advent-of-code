-- https://adventofcode.com/2015/day/15
module Day15 (solve) where
import           AOC.Prelude
import           Data.List (maximum)
import           AOC (aoc)
import           AOC.Parser (Parser, letterChar, sepEndBy1, signedDecimal, some, eol, format)

type Properties = ([Int], Int) -- [capacity, durability, flavor, texture], calories

parser :: Parser [Properties]
parser = properties `sepEndBy1` eol where
    properties= do
        (_, a,b, c, d', e) <- 
            [format|{name}: capacity {d}, durability {d}, flavor {d}, texture {d}, calories {d}|]
        pure ([a, b, c, d'], e)
    d = signedDecimal
    name = some letterChar

integerPartitions :: Int -> Int -> [[Int]]
integerPartitions 1 n = [[n]]
integerPartitions k n = [0..n] & concatMap \i -> map (i:) (integerPartitions (k-1) (n-i))

score :: [Int] -> [[Int]] -> Int 
score part = product . map (max 0 . sum . zipWith (*) part)

part1 :: [Properties] -> Int
part1 properties = maximum [ score part tprops
                           | let tprops = transpose (map fst properties)
                           , part <- integerPartitions (length properties) 100
                           ] 

part2 :: [Properties] -> Int
part2 properties = maximum [ score part tprops
                           | let tprops = transpose (map fst properties)
                           , let calories = map snd properties
                           , part <- integerPartitions (length properties) 100
                           , sum (zipWith (*) calories part) == 500
                           ] 

solve :: Text -> IO ()
solve = aoc parser part1 part2