-- https://adventofcode.com/2023/day/3
module AOC2023.Day03 (solve) where
import           RIO hiding (some)
import           RIO.Partial (read)
import           RIO.List.Partial (head, last)
import           RIO.Char (isDigit)
import           Text.Megaparsec (anySingleBut, sepEndBy1, some)
import           Text.Megaparsec.Char (eol)
import           Util (Parser, aoc)
import           Data.List.Extra (wordsBy)
import           Data.Massiv.Array ((!?), fromLists', U, Comp(Seq), Ix2(..))

type NumberRange = ((Int, Int, Int), Int) -- position of a number: row, start column, end column, the scanned number

parser :: Parser [[Char]]
parser = some (anySingleBut '\n') `sepEndBy1` eol

findNumberRanges :: [[Char]] -> [NumberRange]
findNumberRanges grid = concatMap rangesInRow (zip [0..] grid) where
    rangesInRow (idx, row) = [ ((idx, fst (head w), fst (last w)), read @Int (map snd w))
                           | w <- wordsBy (not . isDigit . snd) (zip [0..] row)
                           ]

part1 :: [[Char]] -> Int
part1 grid = sum [n | (range, n) <- findNumberRanges grid, checkBorders range]  where
    checkBorders (row, start, end) =
        any (maybe False isSymbol) [ mat !? Ix2 r c
                                   | r <- [row-1..row+1]
                                   , c <- [start-1..end+1]
                                   ]
    isSymbol ch = not (isDigit ch || ch == '.')
    mat = fromLists' @U Seq grid

part2 :: [[Char]] -> Int
part2 grid = sum . map gearOf $ starPositions where
    gearOf (row, column) = case adjacentRanges row column of
        [(_, x), (_, y)] -> x * y
        _ -> 0
    adjacentRanges r c = filter (isAdjacentTo r c) ranges 
    isAdjacentTo r c ((row, start, end), _) = row-1 <= r && r <= row+1 && start-1 <= c && c <= end+1
    ranges = findNumberRanges grid
    starPositions = [ (r, c)
                    | (r, row) <- zip [0..] grid
                    , (c, ch) <- zip [0..] row
                    , ch == '*'
                    ]

solve :: MonadIO m => Text -> m ()
solve = aoc parser part1 part2