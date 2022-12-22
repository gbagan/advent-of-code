-- https://adventofcode.com/2022/day/12
module AOC2022.Day12 (solve) where
import           RIO hiding (some)
import           RIO.Char (ord)
import           RIO.List (find)
import           Text.Megaparsec (sepEndBy1, some)
import           Text.Megaparsec.Char (letterChar, eol)
import           Util (Parser, aoc, adjacentPoints)
import           Util.Matrix (Matrix)
import qualified Util.Matrix as M
import           Util.Search (distance)

parser :: Parser (Matrix Char)
parser = M.fromList <$> some letterChar `sepEndBy1` eol

solve' :: Char -> Matrix Char -> Maybe Int
solve' target m = do
    (sourcex, sourcey, _) <- find (\(_, _, c) -> c == 'E') (M.elemsWithIndex m)
    distance nbors (\xy -> m M.!? xy == Just target) (sourcex, sourcey)
    where
    nbors xy = filter (canClimb xy) (adjacentPoints xy)
    canClimb xy xy' = case (m M.!? xy, m M.!? xy') of
                            (Just v, Just v') | v /= 'E' && ord v - ord v' <= 1
                                || [v, v'] `elem` ["Ez", "Ey", "aS"] -> True
                            _ -> False

solve :: MonadIO m => Text -> m ()
solve = aoc parser (solve' 'S') (solve' 'a')