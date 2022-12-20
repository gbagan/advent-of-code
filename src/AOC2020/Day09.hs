-- https://adventofcode.com/2020/day/9
module AOC2020.Day09 (solve) where
import           RIO
import           RIO.List (find)
import           RIO.List.Partial (minimum, maximum)
import qualified RIO.Vector as V
import qualified RIO.Vector.Partial as V ((!))
import qualified Data.IntSet as Set
import           Data.List.Split (divvy)
import           Data.Array (assocs, listArray, range, (!))
import           Text.Megaparsec (sepEndBy1)
import           Text.Megaparsec.Char (eol)
import           Text.Megaparsec.Char.Lexer (decimal)
import           Util (Parser, aoc)

parser :: Parser [Int]
parser = decimal `sepEndBy1` eol

part1 :: [Int] -> Maybe Int
part1 = listToMaybe <=< (find test . divvy 26 1 . reverse) where
    test [] = False
    test (x:xs) = xs & all \y -> y+y == x || (x - y) `Set.notMember` s where
        s = Set.fromList xs

part2 :: [Int] -> Maybe Int
part2 l = do
    val <- part1 l   
    (a, b) <- listToMaybe [(a, b) | ((a, b), v) <- assocs sums, v == val]
    let vec' = V.slice a (b - a + 1) vec
    pure $ minimum vec' + maximum vec'
    where
    sums = listArray bds
        [if | i > j -> 0
            | i == j -> vec V.! i
            | otherwise -> sums ! (i, j-1) + vec V.! j
        | (i, j) <- range bds
        ]
    bds = ((0, 0), (n-1, n-1))
    vec = V.fromList l :: Vector Int
    n = V.length vec

solve :: MonadIO m => Text -> m ()
solve = aoc parser part1 part2