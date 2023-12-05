-- https://adventofcode.com/2023/day/5
module AOC2023.Day05 (solve) where
import           RIO hiding (some)
import           RIO.List (find)
import           RIO.List.Partial (minimum)
import           RIO.Partial (fromJust) 
import           Text.Megaparsec (anySingleBut, sepBy1, sepEndBy1, some)
import           Text.Megaparsec.Char (char, string, eol)
import           Text.Megaparsec.Char.Lexer (decimal)
import           Util (Parser, aoc)

type AMap = [(Int, Int, Int)]
data Almanac = Almanac [Int] [AMap]

parser :: Parser Almanac
parser = Almanac <$> seeds <* eol <* eol <*> amap `sepEndBy1` eol where
    seeds = string "seeds: " *> decimal `sepBy1` char ' '
    amap = do
        _ <- some (anySingleBut '\n')
        _ <- eol
        range `sepEndBy1` eol
    range = (,,) <$> decimal <* char ' ' <*> decimal <* char ' ' <*> decimal


travel :: [AMap] -> Int -> Int
travel maps seed = foldl' destFromSource seed maps where
    destFromSource x ranges =
        case [dest - source + x | (dest, source, len) <- ranges, source <= x && x < source + len] of
            [] -> x
            (d:_) -> d

part1 :: Almanac -> Int
part1 (Almanac seeds maps) = minimum $ map (travel maps) seeds

pairwise :: [a] -> [(a, a)]
pairwise [] = []
pairwise [_] = []
pairwise (x:y:xs) = (x, y) : pairwise xs

part2 :: Almanac -> Maybe Int
part2 (Almanac seeds maps) = find isValid [approx-1000..approx+1000] where
    approx = fromJust $ find isValid [0,1000..]
    isValid = isSeed . travel reverseMaps
    seeds' = [(start, start+len-1) | (start, len) <- pairwise seeds]
    isSeed x = seeds' & any \(start, end) -> start <= x && x <= end
    reverseMaps = reverse . map (map reverseMap) $ maps
    reverseMap (dest, source, len) = (source, dest, len)

solve :: MonadIO m => Text -> m ()
solve = aoc parser part1 part2