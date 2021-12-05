{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}

module Main where
import Data.Maybe (mapMaybe)
import Data.List (foldl')
import Text.Regex.Applicative
import Text.Regex.Applicative.Common (decimal)
import Data.Map.Strict (Map)
import qualified Data.Map as Map

type Regex = RE Char

data Line = Line (Int, Int) (Int, Int)
type Board = Map (Int, Int) Int

coord :: Regex (Int, Int)
coord = (,) <$> decimal <* sym ',' <*> decimal

line :: Regex Line
line = Line <$> coord <* string " -> " <*> coord

playOn :: (Int, Int) -> Board -> Board
playOn (x, y) = Map.insertWith (+) (x, y) 1

fillBoard1 :: Bool -> Line -> Board -> Board
fillBoard1 diag (Line (x1, y1) (x2, y2)) board
    | y1 == y2  = foldl' (flip playOn) board ((,y1) <$> [min x1 x2..max x1 x2])
    | x1 == x2  = foldl' (flip playOn) board ((x1,) <$> [min y1 y2..max y1 y2])
    | diag      = let len = abs (x1 - x2)
                      dx = signum (x2 - x1)
                      dy = signum (y2 - y1)
                      move (x, y) = (x + dx, y + dy)
                  in foldl' (flip playOn) board $ take (len+1) (iterate move (x1, y1))
    | otherwise = board

fillBoard :: Bool -> [Line] -> Board
fillBoard diag = foldl' (flip (fillBoard1 diag)) Map.empty

countIntersections :: Bool -> [Line] -> Int
countIntersections diag ls = 
    length . filter (\(k, v) -> v > 1) . Map.toList . fillBoard diag $ ls

main = do
    ls <- mapMaybe (=~ line) . lines <$> readFile "data05"
    print $ countIntersections False ls
    print $ countIntersections True ls