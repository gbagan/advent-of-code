-- https://adventofcode.com/2018/day/5
module Day05 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Parser (Parser, some, letterChar)
import           Data.Char (toLower)
import           Data.List (minimum)

parser :: Parser String
parser = some letterChar

simplify :: String -> String
simplify = go [] where
    go stack [] = stack
    go [] (x:xs) = go [x] xs
    go (s:ss) (x:xs) | toLower s == toLower x && s /= x = go ss xs
                     | otherwise = go (x:s:ss) xs

part1 :: String -> Int
part1 = length . simplify

part2 :: String -> Int
part2 xs = minimum [part1 $ filter (\c' -> toLower c' /= c) ys | c <- ['a'..'z']] where
    ys = simplify xs

solve :: Text -> IO ()
solve = aoc parser part1 part2