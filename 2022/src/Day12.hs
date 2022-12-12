-- https://adventofcode.com/2022/day/12
module Day12 (solve) where
import           RIO hiding (some)
import           RIO.Char (ord)
import           RIO.List (find)
import           Text.Megaparsec (sepEndBy1, some)
import           Text.Megaparsec.Char (letterChar, eol)
import           Util (Parser, aoc, adjacentPoints, dijkstra)
import           Util.Matrix (Matrix)
import qualified Util.Matrix as M

parser :: Parser (Matrix Char)
parser = M.fromList <$> some letterChar `sepEndBy1` eol

solve' :: Char -> Matrix Char -> Maybe Int
solve' target m = do
    (sourcex, sourcey, _) <- find (\(_, _, c) -> c == 'E') (M.elemsWithIndex m)
    dijkstra nbors (sourcex, sourcey) (\xy -> m M.!? xy == Just target)
    where
    nbors xy = [(xy', 1) | xy' <- adjacentPoints xy, canClimb xy xy']
    canClimb xy xy' = case (m M.!? xy, m M.!? xy') of
                            (Just v, Just v') | v /= 'E' && ord v - ord v' <= 1
                                || [v, v'] `elem` ["Ez", "Ey", "aS"] -> True
                            _ -> False

solve :: (HasLogFunc env) => Text -> RIO env ()
solve = aoc parser (solve' 'S') (solve' 'a')