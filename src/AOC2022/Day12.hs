-- https://adventofcode.com/2022/day/12
module AOC2022.Day12 (solve) where
import           RIO hiding (some)
import           RIO.Char (ord)
import           RIO.List (find)
import           Text.Megaparsec (sepEndBy1, some)
import           Text.Megaparsec.Char (letterChar, eol)

import           Data.Massiv.Array (Matrix, (!?), fromLists', toLists2, U, Comp(Seq), Ix2(..))
import           Util (Parser, aoc, adjacentPoints, flattenWithIndex)
import           Util.Search (distance)

parser :: Parser (Matrix U Char)
parser = fromLists' Seq <$> some letterChar `sepEndBy1` eol

solveWith :: Char -> Matrix U Char -> Maybe Int
solveWith target m = do
    (sourcex, sourcey, _) <- find (\(_, _, c) -> c == 'E') (flattenWithIndex $ toLists2 m)
    distance nbors (\(x, y) -> (m !? Ix2 x y) == Just target) (sourcex, sourcey)
    where
    nbors xy = filter (canClimb xy) (adjacentPoints xy)
    canClimb (x, y) (x', y') = case (m !? Ix2 x y, m !? Ix2 x' y') of
                                     (Just v, Just v') | v /= 'E' && ord v - ord v' <= 1
                                       || [v, v'] `elem` ["Ez", "Ey", "aS"] -> True
                                     _ -> False

solve :: MonadIO m => Text -> m ()
solve = aoc parser (solveWith 'S') (solveWith 'a')