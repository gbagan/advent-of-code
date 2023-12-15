-- https://adventofcode.com/2022/day/12
module AOC2022.Day12 (solve) where
import           AOC.Prelude
import           Data.Massiv.Array (Matrix, (!?), fromLists', toLists2, U, Comp(Seq))
import           AOC (aoc)
import           AOC.Parser (Parser, sepEndBy1, some, eol, letterChar, eol)
import           AOC.Search (distance)
import           AOC.List (flattenWithIndex)
import           AOC.V2 (V2(..), adjacent, toIx2)

parser :: Parser (Matrix U Char)
parser = fromLists' Seq <$> some letterChar `sepEndBy1` eol

solveFor :: Char -> Matrix U Char -> Maybe Int
solveFor target m = do
    (sourcex, sourcey, _) <- find (\(_, _, c) -> c == 'E') (flattenWithIndex (toLists2 m))
    distance nbors (\p -> (m !? toIx2 p) == Just target) (V2 sourcex sourcey)
    where
    nbors xy = filter (canClimb xy) (adjacent xy)
    canClimb p1 p2 = case (m !? toIx2 p1, m !? toIx2 p2) of
        (Just v, Just v') | v /= 'E' && ord v - ord v' <= 1
          || [v, v'] `elem` ["Ez", "Ey", "aS"] -> True
        _ -> False

solve :: Text -> IO ()
solve = aoc parser (solveFor 'S') (solveFor 'a')