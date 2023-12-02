-- https://adventofcode.com/2023/day/3
module AOC2023.Day03 (solve) where
import           RIO hiding (some)
import           RIO.Partial (read)
import           RIO.List.Partial (head, last)
import           RIO.Char (isDigit)
import           Text.Megaparsec (anySingleBut, sepEndBy1, some)
import           Text.Megaparsec.Char (eol)
import           Util (Parser, aoc)
import           Util.Matrix ((!?), fromList)

type NumberRange = (Int, Int, Int, Int) -- position of a number: row, start column, end column, the scanned number

parser :: Parser [[Char]]
parser = some (anySingleBut '\n') `sepEndBy1` eol

findNumberRanges :: [[Char]] -> [NumberRange]
findNumberRanges table = concatMap rangesInRow (zip [0..] table) where
    rangesInRow (i, row) = go i (zip [0..] row)
    go i js =
        case break (isDigit . snd) js of
            (_, []) -> []
            (_, after) -> case span (isDigit . snd) after of
                (before', after') -> (i, fst (head before'), fst (last before'), read @Int $ map snd before') : go i after'

solve1 :: [[Char]] -> Int
solve1 table = sum . map (\(_, _, _, n) -> n) . filter checkBorders $ findNumberRanges table where
    checkBorders (row, start, end, _) = any (maybe False isSymbol) [mat !? (r, c) | r <- [row-1..row+1], c <- [start-1..end+1]]
    isSymbol ch = not (isDigit ch || ch == '.')
    mat = fromList table

solve2 :: [[Char]] -> Int
solve2 table = sum (map gear positions) where
    gear (r, c, _) = case adjacentRanges r c of
        [(_, _, _, x), (_, _, _, y)] -> x * y
        _ -> 0
    adjacentRanges r c = ranges & filter \(row, start, end, _) -> row-1 <= r && r <= row+1 && start-1 <= c && c <= end+1
    ranges = findNumberRanges table
    positions = [(r, c, ch) | (r, row) <- zip [0..] table, (c, ch) <- zip [0..] row, ch == '*']

solve :: MonadIO m => Text -> m ()
solve = aoc parser solve1 solve2