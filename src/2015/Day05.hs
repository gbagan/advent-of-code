-- https://adventofcode.com/2015/day/5
module Day05 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Parser (Parser, eol, some, lowerChar, sepEndBy1)
import           AOC.List (count, isInfixOf)

parser :: Parser [String]
parser = some lowerChar `sepEndBy1` eol

check1 :: String -> Bool
check1 s = count (`elem` ['a', 'e', 'i', 'o', 'u']) s >= 3
        && any ((>=2) . length) (group s)
        && not (any ( `isInfixOf` s) ["ab", "cd", "pq", "xy"])

check2 :: String -> Bool
check2 s = check21 s && check22 s where
    check21 (x : y : xs) = [x, y] `isInfixOf` xs || check21 (y : xs)
    check21 _ = False
    check22 (x : y : z : xs) = x == z || check22 (y : z : xs) 
    check22 _ = False

solve :: Text -> IO ()
solve = aoc parser (count check1) (count check2)