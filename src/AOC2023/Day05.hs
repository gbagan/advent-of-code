-- https://adventofcode.com/2023/day/5
module AOC2023.Day05 (solve) where
import           AOC.Prelude
import           Data.List (minimum)
import           AOC (aoc)
import           AOC.Parser (Parser, sepBy1, sepEndBy1, eol, decimal, skipLine)
import           AOC.Tuple (both, snd3, thd3)

type Interval = (Int, Int) -- start, end
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

solve' :: [AMap] -> [Interval] -> Int
solve' maps seedIntervals = minimum (map fst finalIntervals) where
    finalIntervals = foldl' go seedIntervals maps
    go intervals ranges = catMaybes [nextInterval range itv | itv <- intervals, range <- fillRanges ranges]
    nextInterval (dest, source, len) (begin, end) 
        | end < source = Nothing
        | source + len - 1 < begin = Nothing
        | otherwise = Just $ both (+ (dest - source)) (max begin source, min end (source + len - 1))
    fillRanges = filter ((/= 0) . thd3) . fillRanges' 0 . sortOn snd3
    fillRanges' start [] = [(start, start, maxBound)]
    fillRanges' start (x@(_, source, len) : xs) = (start, start, source-start) : x : fillRanges' (source+len) xs

part1 :: Almanac -> Int
part1 (Almanac seeds maps) = solve' maps [(s, s) | s <- seeds]

part2 :: Almanac -> Int
part2 (Almanac seeds maps) = solve' maps [(start, start+len-1) | (start, len) <- chunksOf2 seeds]

solve :: Text -> IO ()
solve = aoc parser part1 part2