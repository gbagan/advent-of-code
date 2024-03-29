-- https://adventofcode.com/2023/day/5
module Day05 (solve) where
import           AOC.Prelude hiding (Map)
import           AOC (aoc)
import           AOC.Range (Range(..), intersection, translate)
import           AOC.List (minimumMaybe)
import           AOC.Parser (Parser, sepBy1, sepEndBy1, eol, decimal, skipLine)
import           AOC.Tuple (snd3, thd3)

type Range' = (Int, Int, Int) -- destination, source, length
type Map = [Range']
data Almanac = Almanac [Int] [Map] -- seeds, maps

parser :: Parser Almanac
parser = Almanac <$> seeds <* eol <* eol <*> amap `sepEndBy1` eol where
    seeds = "seeds: " *> decimal `sepBy1` " "
    amap = skipLine *> range `sepEndBy1` eol
    range = (,,) <$> decimal <* " " <*> decimal <* " " <*> decimal

chunksOf2 :: [a] -> [(a, a)]
chunksOf2 (x:y:xs) = (x, y) : chunksOf2 xs
chunksOf2 _ = []

fillRanges :: [Range'] -> [Range']
fillRanges = filter ((/= 0) . thd3) . fillRanges' 0 . sortOn snd3 where
    fillRanges' start [] = [(start, start, maxBound)]
    fillRanges' start (x@(_, source, len) : xs) = (start, start, source-start) : x : fillRanges' (source+len) xs

solve' :: [Map] -> [Range Int] -> Maybe Int
solve' maps seedIntervals = minimumMaybe (map _lower finalIntervals) where
    finalIntervals = foldl' go seedIntervals maps
    go intervals ranges = catMaybes (nextInterval <$> fillRanges ranges <*> intervals)
    nextInterval (dest, source, len) itv = do
        inter <- intersection itv (Range source (source + len - 1))
        Just $ translate (dest - source) inter

part1 :: Almanac -> Maybe Int
part1 (Almanac seeds maps) = solve' maps [Range s s | s <- seeds]

part2 :: Almanac -> Maybe Int
part2 (Almanac seeds maps) = solve' maps [Range start (start+len-1) | (start, len) <- chunksOf2 seeds]

solve :: Text -> IO ()
solve = aoc parser part1 part2