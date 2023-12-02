-- https://adventofcode.com/2023/day/3
module AOC2023.Day03 (solve) where
import           RIO hiding (some)
import           RIO.Partial (read)
import           RIO.Char (isDigit)
import           Text.Megaparsec (anySingleBut, sepEndBy1, some)
import           Text.Megaparsec.Char (eol)
import           Util (Parser, aoc)
import qualified Util.Matrix as M

type Token = (Int, Int, Int)

parser :: Parser (M.Matrix Char)
parser = M.fromList <$> some (anySingleBut '\n') `sepEndBy1` eol

findTokens :: M.Matrix Char -> [Token]
findTokens mat = concatMap tokensInRow $ [0..m-1] where
    tokensInRow i = reverse $ foldl' (go i) [] [0..n-1]
    go i tokens j =
        let c = mat M.! (i, j) in
        case tokens of
            [] | isDigit c -> [(i, j, j)]
               | otherwise -> []
            (row, start, end):ts | isDigit c && end == j-1 -> (row, start, end+1) : ts
                                 | isDigit c -> (row, j, j):tokens
                                 | otherwise -> tokens
    n = M.nbColumns mat
    m = M.nbRows mat

extractNumber :: M.Matrix Char -> Token -> Int
extractNumber mat (row, start, end) = read @Int [mat M.! (row, c) | c <- [start..end]]

solve1 :: M.Matrix Char -> Int
solve1 mat = sum . map (extractNumber mat) . filter checkBorders $ findTokens mat where
    checkBorders (row, start, end) = any (maybe False isSymbol) [mat M.!? (r, c) | r <- [row-1..row+1], c <- [start-1..end+1]]
    isSymbol ch = not (isDigit ch || ch == '.')

solve2 :: M.Matrix Char -> Int
solve2 mat = sum (map gear positions) where
    gear (r, c) = case adjacentTokens (r, c) of
        [x, y] -> extractNumber mat x * extractNumber mat y
        _ -> 0
    adjacentTokens (r, c) = tokens & filter \(row, start, end) -> mat M.! (r, c) == '*' && row-1 <= r && r <= row+1 && start-1 <= c && c <= end+1
    tokens = findTokens mat
    positions = [(r, c) | r <- [0..m-1], c <- [0..n-1]]
    n = M.nbColumns mat
    m = M.nbRows mat

solve :: MonadIO m => Text -> m ()
solve = aoc parser solve1 solve2