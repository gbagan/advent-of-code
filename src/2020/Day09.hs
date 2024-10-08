-- https://adventofcode.com/2020/day/9
module Day09 (solve) where
import           AOC.Prelude
import           Data.List (minimum, maximum)
import qualified Data.IntSet as Set
import           AOC (aoc)
import           AOC.List (headMaybe, slice, sliding)
import           AOC.Parser (Parser, sepEndBy1, eol, decimal)

parser :: Parser [Int]
parser = decimal `sepEndBy1` eol

part1 :: [Int] -> Maybe Int
part1 = headMaybe <=< (find test . sliding 26 . reverse) where
    test [] = False
    test (x:xs) = xs & all \y -> y+y == x || (x - y) `Set.notMember` s where
        s = Set.fromList xs

part2 :: [Int] -> Maybe Int
part2 l = do
    val <- part1 l 
    let sums = zip [0..] $ scanl' (+) 0 l
    (a, b) <- headMaybe [ (i1, i2) 
                        | (i1, v1) <- sums
                        , (i2, v2) <- sums
                        , i1 < i2
                        , v2 - v1 == val
                        ]
    let range = slice a (b-1) l
    pure $ minimum range + maximum range

solve :: Text -> IO ()
solve = aoc parser part1 part2