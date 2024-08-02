-- https://adventofcode.com/2018/day/14
module Day14 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Parser (decimal)
import qualified Data.Sequence as Seq
import           Data.Char (digitToInt)

toDigits :: Int -> [Int]
toDigits = map digitToInt . show

recipes :: [Int]
recipes = 3 : 7 : go (Seq.fromList [3, 7]) 0 1 where
    go !seq_ !idx1 !idx2 = digits ++ go seq' idx1' idx2' where
        score1 = seq_ `Seq.index` idx1
        score2 = seq_ `Seq.index` idx2
        digits = toDigits (score1 + score2)
        seq' = seq_ <> Seq.fromList digits
        idx1' = (idx1 + score1 + 1) `rem` Seq.length seq'
        idx2' = (idx2 + score2 + 1) `rem` Seq.length seq'

part1 :: Int -> String
part1 n = concatMap show . take 10 $ drop n recipes

part2 :: Int -> Maybe Int
part2 n = findIndex (pattern_ `isPrefixOf`) (tails recipes) where
    pattern_ = toDigits n

solve :: Text -> IO ()
solve = aoc decimal part1 part2