module Day05 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Parser (Parser, decimal, eol, sepEndBy1)
import           AOC.List (count, freqs)

data Line = Line (Int, Int) (Int, Int)
data Diag = Diag | NoDiag deriving Eq

parser :: Parser [Line]
parser = line `sepEndBy1` eol where
    coord = (,) <$> decimal <* "," <*> decimal
    line = Line <$> coord <* " -> " <*> coord

points :: Diag -> Line -> [(Int, Int)]
points diag (Line (x1, y1) (x2, y2))
    | y1 == y2     = (,y1) <$> [min x1 x2..max x1 x2]
    | x1 == x2     = (x1,) <$> [min y1 y2..max y1 y2]
    | diag == Diag = let len = abs (x1 - x2)
                         dx = signum (x2 - x1)
                         dy = signum (y2 - y1)
                         move (x, y) = (x + dx, y + dy)
                     in take (len+1) (iterate' move (x1, y1))
    | otherwise    = []

countIntersections :: Diag -> [Line] -> Int
countIntersections diag = count (>1) . map snd . freqs . concatMap (points diag)

solve :: Text -> IO ()
solve = aoc parser (countIntersections NoDiag) (countIntersections Diag)