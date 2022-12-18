{-# OPTIONS_GHC -Wno-deprecations #-}
module AOC2021.Day13 (solve) where
import           RIO hiding (fold, some)
import           Data.List (maximum)
import           RIO.List (intercalate)
import           RIO.List.Partial (head)
import qualified RIO.Set as Set
import qualified RIO.Text as Text
import           Text.Megaparsec (sepEndBy1, some)
import           Text.Megaparsec.Char (char, eol, string)
import           Text.Megaparsec.Char.Lexer (decimal)
import           Util (Parser, Point, aoc)

data Axis = X | Y
data Fold = Fold Axis Int
data Input = Input (Set Point) [Fold]

parser :: Parser Input
parser = Input <$> points <* eol <*> sepEndBy1 fold eol where
     points = Set.fromList <$> some point
     point = (,) <$> decimal <* char ',' <*> decimal <* eol
     fold = Fold <$> (string "fold along " *> axis) <* char '=' <*> decimal
     axis = X <$ char 'x' <|> Y <$ char 'y'

foldPaper :: Fold -> Set Point -> Set Point
foldPaper (Fold X i) = Set.map \(x, y) -> (min x (2*i - x), y)
foldPaper (Fold Y i) = Set.map \(x, y) -> (x, min y (2*i - y))

part1 :: Input -> Int
part1 (Input paper folds) = Set.size $ foldPaper (head folds) paper

part2 :: Input -> Int
part2 (Input paper folds) = trace (Text.pack str) 0 where
    str = intercalate "\n" 
        [
            [if Set.member (x, y) folded then '#' else ' ' | x <- [0..xMax]]
            | y <- [0..yMax]        
        ]
    folded = foldl' (flip foldPaper) paper folds
    xMax = maximum (Set.map fst folded)
    yMax = maximum (Set.map snd folded)

solve :: MonadIO m => Text -> m ()
solve = aoc parser part1 part2