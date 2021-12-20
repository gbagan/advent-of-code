module Day05 (solve) where
import qualified Data.Map as Map
import           Text.Megaparsec (sepEndBy1)
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import           Util (Parser, aocTemplate, freqs)

data Line = Line (Int, Int) (Int, Int)
data Diag = Diag | NoDiag deriving Eq

parser :: Parser [Line]
parser = line `sepEndBy1` P.eol where
    coord = (,) <$> L.decimal <* P.char ',' <*> L.decimal
    line = Line <$> coord <* P.string " -> " <*> coord

points :: Diag -> Line -> [(Int, Int)]
points diag (Line (x1, y1) (x2, y2))
    | y1 == y2     = (,y1) <$> [min x1 x2..max x1 x2]
    | x1 == x2     = (x1,) <$> [min y1 y2..max y1 y2]
    | diag == Diag = let len = abs (x1 - x2)
                         dx = signum (x2 - x1)
                         dy = signum (y2 - y1)
                         move (x, y) = (x + dx, y + dy)
                     in take (len+1) (iterate move (x1, y1))
    | otherwise    = []

countIntersections :: Diag -> [Line] -> Int
countIntersections diag = Map.size . Map.filter (>1) . freqs . (>>= points diag)

solve :: String -> IO ()
solve = aocTemplate parser pure (pure . countIntersections NoDiag) (pure . countIntersections Diag)