-- https://adventofcode.com/2023/day/5
module AOC2023.Day05 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Interval (Interval(..), intersection, translate)
import           AOC.List (minimumMaybe)
import           AOC.Parser (Parser, sepBy1, sepEndBy1, eol, decimal, skipLine)
import           AOC.Tuple (snd3, thd3)

type Range = (Int, Int, Int) -- destination, source, length
type AMap = [Range]
data Almanac = Almanac [Int] [AMap] -- seeds, maps

parser :: Parser Almanac
parser = Almanac <$> seeds <* eol <* eol <*> amap `sepEndBy1` eol where
    seeds = "seeds: " *> decimal `sepBy1` " "
    amap = skipLine *> range `sepEndBy1` eol
    range = (,,) <$> decimal <* " " <*> decimal <* " " <*> decimal

chunksOf2 :: [a] -> [(a, a)]
chunksOf2 (x:y:xs) = (x, y) : chunksOf2 xs
chunksOf2 _ = []

fillRanges :: [Range] -> [Range]
fillRanges = filter ((/= 0) . thd3) . fillRanges' 0 . sortOn snd3 where
    fillRanges' start [] = [(start, start, maxBound)]
    fillRanges' start (x@(_, source, len) : xs) = (start, start, source-start) : x : fillRanges' (source+len) xs

solve' :: [AMap] -> [Interval Int] -> Maybe Int
solve' maps seedIntervals = minimumMaybe (map _start finalIntervals) where
    finalIntervals = foldl' go seedIntervals maps
    go intervals ranges = catMaybes (nextInterval <$> fillRanges ranges <*> intervals)
    nextInterval (dest, source, len) itv = do
        inter <- intersection itv (Interval source (source + len - 1))
        Just $ translate (dest - source) inter

part1 :: Almanac -> Maybe Int
part1 (Almanac seeds maps) = solve' maps [Interval s s | s <- seeds]

part2 :: Almanac -> Maybe Int
part2 (Almanac seeds maps) = solve' maps [Interval start (start+len-1) | (start, len) <- chunksOf2 seeds]

solve :: Text -> IO ()
solve = aoc parser part1 part2