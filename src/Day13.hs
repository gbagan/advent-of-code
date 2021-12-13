module Day13 (solve) where
import           Data.List (foldl', intercalate)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Void (Void)
import           Text.Megaparsec (Parsec, parseMaybe, sepEndBy1, some, (<|>))
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import           Util (Point)
import           Debug.Trace (traceM)

type Parser = Parsec Void String
data Axis = X | Y
data Fold = Fold Axis Int
data Input = Input (Set Point) [Fold]

parser :: Parser Input
parser = Input <$> points <* P.eol <*> some fold where
     points = Set.fromList <$> some point
     fold = Fold <$> (P.string "fold along " *> axis) <* P.char '=' <*> L.decimal <* P.eol
     axis = X <$ P.char 'x' <|> Y <$ P.char 'y'
     point = (,) <$> L.decimal <* P.char ',' <*> L.decimal <* P.eol

foldPaper :: Fold -> Set Point -> Set Point
foldPaper (Fold X i) = Set.map \(x, y) -> (min x (2*i - x), y)
foldPaper (Fold Y i) = Set.map \(x, y) -> (x, min y (2*i - y))

part1 :: Input -> Int
part1 (Input paper folds) = Set.size $ foldPaper (head folds) paper

part2 :: Input -> String
part2 (Input paper folds) =
    intercalate "\n" 
        [
            [if Set.member (x, y) folded then '#' else ' ' | x <- [0..xMax]]
            | y <- [0..yMax]        
        ] where
    folded = foldl' (flip foldPaper) paper folds
    xMax = maximum (Set.map fst folded)
    yMax = maximum (Set.map snd folded)

solve :: String -> Maybe (Int, Int)
solve s = do
    input <- parseMaybe parser s
    traceM $ part2 input
    pure (part1 input, 0)