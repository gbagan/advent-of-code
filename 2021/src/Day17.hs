module Day17 (solve) where
import           RIO
import           RIO.List (iterate)
import           Text.Megaparsec.Char (string)
import           Text.Megaparsec.Char.Lexer (decimal)
import           Util (Parser, aoc, cartesianProduct, count)

data Area = Area Int Int Int Int

parser :: Parser Area
parser = Area <$> (string "target area: x=" *> decimal) <* string ".." <*> decimal <*
                    string ", y=-" <*> (negate <$> decimal) <* string "..-" <*> (negate <$> decimal)

part1 :: Area -> Int 
part1 (Area _ _ ymin _) = (-ymin) * (-ymin -1) `div` 2 

simulate :: Area -> (Int, Int) -> Bool
simulate (Area xmin xmax ymin ymax) (vx, vy) = 
    any (inArea . fst) . takeWhile (\((_, y), _) -> y >= ymin) $ run where
    inArea (x, y) = xmin <= x && x <= xmax && ymin <= y && y <= ymax
    step ((x, y), (dx, dy)) = ((x+dx, y+dy), (max (dx-1) 0, dy-1))
    run = iterate step ((0, 0), (vx, vy))

part2 :: Area -> Int
part2 area@(Area _ xmax ymin _) = count (simulate area) (cartesianProduct [1..xmax] [ymin..(-ymin)])

solve :: (HasLogFunc env) => Text -> RIO env ()
solve = aoc parser part1 part2
