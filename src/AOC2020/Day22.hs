-- https://adventofcode.com/2020/day/22
module Day22 (solve) where
import           AOC.Prelude
import           Data.Sequence (Seq(..), (><))
import qualified Data.Sequence as Seq
import           AOC (aoc)
import           AOC.Parser (Parser, sepEndBy1, eol, decimal)

parser :: Parser ([Int], [Int])
parser = do
    list1 <- "Player 1:" *> eol *> decimal `sepEndBy1` eol
    list2 <- eol *> "Player 2:" *> eol *> decimal `sepEndBy1` eol
    pure (list1, list2)

score :: Seq Int -> Int
score = sum . zipWith (*) [1..] . reverse . toList

part1 :: ([Int], [Int]) -> Int
part1 (xs, ys) = go (Seq.fromList xs) (Seq.fromList ys) where
    go Empty ys' = score ys'
    go xs' Empty = score xs'
    go (x :<| xs') (y :<| ys') | x < y      = go xs' (ys' >< Seq.fromList [y, x])
                                | otherwise = go (xs' >< Seq.fromList [x, y]) ys'

part2 :: ([Int], [Int]) -> Int
part2 _ = 0

solve :: Text -> IO ()
solve = aoc parser part1 part2