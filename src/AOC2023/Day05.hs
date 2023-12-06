-- https://adventofcode.com/2023/day/5
module AOC2023.Day05 (solve) where
import           RIO hiding (some)
import           RIO.List (sortOn)
import           RIO.List.Partial (minimum)
import           Data.Tuple.Extra (both, snd3, thd3)
import           Text.Megaparsec (sepBy1, sepEndBy1)
import           Text.Megaparsec.Char (eol)
import           Text.Megaparsec.Char.Lexer (decimal)
import           Util (Parser, aoc)
import           Util.Parser (skipLine)

type Interval = (Int, Int)
type Range = (Int, Int, Int)
type AMap = [Range]
data Almanac = Almanac [Int] [AMap]

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

solve :: MonadIO m => Text -> m ()
solve = aoc parser part1 part2




{-
travel :: [AMap] -> Int -> Int
travel maps seed = foldl' destFromSource seed maps where
    destFromSource x ranges =
        case [dest - source + x | (dest, source, len) <- ranges, source <= x && x < source + len] of
            [] -> x
            (d:_) -> d

part1 :: Almanac -> Int
part1 (Almanac seeds maps) = minimum $ map (travel maps) seeds

part2 :: Almanac -> Maybe Integer
part2 (Almanac seeds maps) = find isValid [approx-1000..approx+1000] where
    approx = fromJust $ find isValid [0,1000..]
    isValid = isSeed . travel reverseMaps
    seeds' = [(start, start+len-1) | (start, len) <- pairwise seeds]
    isSeed x = seeds' & any \(start, end) -> start <= x && x <= end
    reverseMaps = reverse . map (map reverseMap) $ maps
    reverseMap (dest, source, len) = (source, dest, len)
-}