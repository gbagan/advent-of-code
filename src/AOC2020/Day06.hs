-- https://adventofcode.com/2020/day/3
module AOC2020.Day06 (solve) where
import           RIO hiding (some)
import           RIO.List.Partial (foldl1')
import qualified RIO.HashSet as Set
import           Text.Megaparsec (sepEndBy1, some)
import           Text.Megaparsec.Char (lowerChar, eol)
import           Util (Parser, aoc)

parser :: Parser [[[Char]]]
parser = group `sepEndBy1` eol where
    group = person `sepEndBy1` eol
    person = some lowerChar

part1 :: [[[Char]]] -> Int
part1 = sum . map (Set.size . Set.fromList . concat)

part2 :: [[[Char]]] -> Int
part2 = sum . map (Set.size . foldl1' Set.intersection . map Set.fromList)

solve :: MonadIO m => Text -> m ()
solve = aoc parser part1 part2
