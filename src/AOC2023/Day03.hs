-- https://adventofcode.com/2023/day/3
module AOC2023.Day03 (solve) where
import           RIO hiding (some)
import           RIO.Partial (read)
import           RIO.Char (isDigit)
import           Text.Megaparsec (anySingleBut, sepEndBy1, some)
import           Text.Megaparsec.Char (eol)
import           Util (Parser, aoc)
import           Util.Matrix (Matrix, (!), (!?), nbRows, nbColumns, fromList)

type NumberRange = (Int, Int, Int) -- position of a number: row, start column, end column

parser :: Parser (Matrix Char)
parser = fromList <$> some (anySingleBut '\n') `sepEndBy1` eol

findNumberRanges :: Matrix Char -> [NumberRange]
findNumberRanges mat = concatMap rangesInRow [0..m-1] where
    rangesInRow i = reverse $ foldl' (go i) [] [0..n-1]
    go i ranges j =
        let c = mat ! (i, j) in
        case ranges of
            [] | isDigit c -> [(i, j, j)]
               | otherwise -> []
            (_, start, end):ts | isDigit c && end == j-1 -> (i, start, end+1) : ts
                               | isDigit c -> (i, j, j) : ranges
                               | otherwise -> ranges
    n = nbColumns mat
    m = nbRows mat

extractNumber :: Matrix Char -> NumberRange -> Int
extractNumber mat (row, start, end) = read @Int [mat ! (row, c) | c <- [start..end]]

solve1 :: Matrix Char -> Int
solve1 mat = sum . map (extractNumber mat) . filter checkBorders $ findNumberRanges mat where
    checkBorders (row, start, end) = any (maybe False isSymbol) [mat !? (r, c) | r <- [row-1..row+1], c <- [start-1..end+1]]
    isSymbol ch = not (isDigit ch || ch == '.')

solve2 :: Matrix Char -> Int
solve2 mat = sum (map gear positions) where
    gear (r, c) = case adjacentRanges (r, c) of
        [x, y] -> extractNumber mat x * extractNumber mat y
        _ -> 0
    adjacentRanges (r, c) = ranges & filter \(row, start, end) -> mat ! (r, c) == '*' && row-1 <= r && r <= row+1 && start-1 <= c && c <= end+1
    ranges = findNumberRanges mat
    positions = [(r, c) | r <- [0..m-1], c <- [0..n-1]]
    n = nbColumns mat
    m = nbRows mat

solve :: MonadIO m => Text -> m ()
solve = aoc parser solve1 solve2