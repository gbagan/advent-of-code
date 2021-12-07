module Day05 (solve) where
import qualified Data.Map as Map
import           Data.Void (Void)
import           Text.Megaparsec (Parsec, parseMaybe, sepEndBy1)
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import           Util (freqs)

type Parser = Parsec Void String

data Line = Line (Int, Int) (Int, Int)
data Diag = Diag | NoDiag deriving Eq

parser :: Parser [Line]
parser = sepEndBy1 lineP P.eol where
    lexeme = L.lexeme P.hspace
    coordP = (,) <$> lexeme L.decimal <* lexeme (P.char ',') <*> lexeme L.decimal
    lineP = Line <$> coordP <* lexeme (P.string "->") <*> coordP

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
countIntersections diag ls = length . Map.filter (>1) . freqs $ ls >>= points diag

solve :: String -> Maybe (Int, Int)
solve s = do
    xs <- parseMaybe parser s
    Just (countIntersections NoDiag xs, countIntersections Diag xs)