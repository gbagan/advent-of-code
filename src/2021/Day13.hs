{-# OPTIONS_GHC -Wno-deprecations #-}
module Day13 (solve) where
import           AOC.Prelude hiding (fold, head, unlines)
import           Data.List (head, maximum, unlines)
import qualified Data.Set as Set
import           AOC (aocIO)
import           AOC.Parser (Parser, decimal, eol, sepEndBy1, some)

type Point = (Int, Int)
data Axis = X | Y
data Fold = Fold Axis Int
data Input = Input (Set Point) [Fold]

parser :: Parser Input
parser = Input <$> points <* eol <*> sepEndBy1 fold eol where
     points = Set.fromList <$> some point
     point = (,) <$> decimal <* "," <*> decimal <* eol
     fold = Fold <$> ("fold along " *> axis) <* "=" <*> decimal
     axis = X <$ "x" <|> Y <$ "y"

foldPaper :: Fold -> Set Point -> Set Point
foldPaper (Fold X i) = Set.map \(x, y) -> (min x (2*i - x), y)
foldPaper (Fold Y i) = Set.map \(x, y) -> (x, min y (2*i - y))

part1 :: Input -> Int
part1 (Input paper folds) = Set.size $ foldPaper (head folds) paper

part2 :: Input -> IO Int
part2 (Input paper folds) = putStr str $> 0 where
    str = unlines
        [
            [if Set.member (x, y) folded then '#' else ' ' | x <- [0..xMax]]
            | y <- [0..yMax]        
        ]
    folded = foldl' (flip foldPaper) paper folds
    xMax = maximum (Set.map fst folded)
    yMax = maximum (Set.map snd folded)

solve :: Text -> IO ()
solve = aocIO parser (pure . part1) part2