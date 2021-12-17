module Day17 (solve) where
import           Data.List (iterate')
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import           Util (Parser, aocTemplate, cartesianProduct, count)

data Area = Area Int Int Int Int

parser :: Parser Area
parser = Area <$> (P.string "target area: x=" *> L.decimal) <* P.string ".." <*> L.decimal <*
                    P.string ", y=-" <*> (negate <$> L.decimal) <* P.string "..-" <*> (negate <$> L.decimal)

part1 :: Area -> Int 
part1 (Area _ _ ymin _) = ((-ymin) * (-ymin -1)) `div` 2 

simulate :: Area -> (Int, Int) -> Bool
simulate (Area xmin xmax ymin ymax) (vx, vy) = 
    any (\(x, y, _, _) -> inZone x y) . takeWhile (\(_, y, _, _) -> y >= ymin) $ run
    where
    inZone x y = xmin <= x && x <= xmax && ymin <= y && y <= ymax
    step (x, y, dx, dy) = (x+dx, y+dy, max (dx-1) 0, dy-1)
    run = iterate' step (0, 0, vx, vy)

part2 :: Area -> Int
part2 area@(Area _ xmax ymin _) = count (simulate area) (cartesianProduct [1..xmax] [ymin..(-ymin)])

solve :: String -> IO ()
solve = aocTemplate parser (pure . part1) (pure . part2)