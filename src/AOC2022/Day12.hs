-- https://adventofcode.com/2022/day/12
module AOC2022.Day12 (solve) where
import           AOC.Prelude
import           Data.Massiv.Array (Matrix, (!?), fromLists', toLists2, U, Comp(Seq), Ix2(..))
import           AOC (aoc)
import           AOC.Parser (Parser, sepEndBy1, some, eol, letterChar, eol)
import           AOC.Search (distance)
import           AOC.Util (adjacentPoints, flattenWithIndex)

parser :: Parser (Matrix U Char)
parser = fromLists' Seq <$> some letterChar `sepEndBy1` eol

solveFor :: Char -> Matrix U Char -> Maybe Int
solveFor target m = do
    (sourcex, sourcey, _) <- find (\(_, _, c) -> c == 'E') (flattenWithIndex $ toLists2 m)
    distance nbors (\(x, y) -> (m !? Ix2 x y) == Just target) (sourcex, sourcey)
    where
    nbors xy = filter (canClimb xy) (adjacentPoints xy)
    canClimb (x, y) (x', y') = case (m !? Ix2 x y, m !? Ix2 x' y') of
                                     (Just v, Just v') | v /= 'E' && ord v - ord v' <= 1
                                       || [v, v'] `elem` ["Ez", "Ey", "aS"] -> True
                                     _ -> False

solve :: Text -> IO ()
solve = aoc parser (solveFor 'S') (solveFor 'a')