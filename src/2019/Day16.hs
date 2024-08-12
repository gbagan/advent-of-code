-- https://adventofcode.com/2019/day/16
module Day16 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Parser (Parser, some, digitChar)
import           Data.Char (digitToInt)
import           AOC.Util (times)
import qualified Data.Vector.Unboxed as V

parser :: Parser (V.Vector Int)
parser = V.fromList <$> some (digitToInt <$> digitChar)

phase :: Int -> V.Vector Int -> V.Vector Int
phase offset vec = V.generate n f where
    n = V.length vec
    psums = V.scanl' (+) 0 vec
    f i | i < offset = 0
        | otherwise = abs $ sum [ val * (psums V.! min n end - psums V.! start) | (start, end, val) <- itvs ] `rem` 10
            where
            itvs = takeWhile (\(start, _, _) -> start < n) (intervals i)

intervals :: Int -> [(Int, Int, Int)]
intervals i = go 0 where
    go m = (m + i, m + 2*i + 1, 1) : (m + 3*i + 2, m + 4*i + 3, -1) : go (m + 4*i + 4)

solveFor :: Int -> V.Vector Int -> String
solveFor offset = concatMap show . V.toList . V.slice offset 8 . times 100 (phase offset)

part1 :: V.Vector Int -> String
part1 = solveFor 0

part2 :: V.Vector Int -> Maybe String
part2 vec = do
    offset <- readMaybe . concatMap show . V.toList $ V.take 7 vec
    pure $ solveFor offset $ V.concat (replicate 10_000 vec)
    

solve :: Text -> IO ()
solve = aoc parser part1 part2