module Day05 (solve) where
import Data.Maybe (mapMaybe)
import Data.List (foldl')
import Text.Regex.Applicative
import Text.Regex.Applicative.Common (decimal)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Regex = RE Char

data Line = Line (Int, Int) (Int, Int)
type Board = Map (Int, Int) Int

coord :: Regex (Int, Int)
coord = (,) <$> decimal <* sym ',' <*> decimal

line :: Regex Line
line = Line <$> coord <* string " -> " <*> coord

playOn :: (Int, Int) -> Board -> Board
playOn (x, y) = Map.insertWith (+) (x, y) 1

fillBoard :: Bool -> [Line] -> Board
fillBoard diag = foldl' go Map.empty where
    go board (Line (x1, y1) (x2, y2)) =
        let points =
             if  | y1 == y2  -> (,y1) <$> [min x1 x2..max x1 x2]
                 | x1 == x2  -> (x1,) <$> [min y1 y2..max y1 y2]
                 | diag      -> let len = abs (x1 - x2)
                                    dx = signum (x2 - x1)
                                    dy = signum (y2 - y1)
                                    move (x, y) = (x + dx, y + dy)
                                in take (len+1) (iterate move (x1, y1))
                 | otherwise -> []
        in foldl' (flip playOn) board points

countIntersections :: Bool -> [Line] -> Int
countIntersections diag ls = 
    length . filter (\(_, v) -> v > 1) . Map.toList . fillBoard diag $ ls

solve :: String -> Maybe (Int, Int)
solve s =
    let ls = mapMaybe (=~ line) . lines $ s in
    Just (countIntersections False ls, countIntersections True ls)