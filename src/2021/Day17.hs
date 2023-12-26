module Day17 (solve) where
import           AOC.Prelude hiding (elem)
import           AOC (aoc)
import           AOC.Parser (Parser, signedDecimal)
import           AOC.List (count)
import           AOC.V2 (V2(..))
import           AOC.Area (Area(..), elem)

parser :: Parser (Area Int)
parser = do
    xmin <- "target area: x=" *> signedDecimal
    xmax <- ".." *> signedDecimal
    ymin <- ", y=" *> signedDecimal
    ymax <- ".." *> signedDecimal
    pure $ Area xmin ymin xmax ymax 

part1 :: Area Int -> Int 
part1 (Area _ ymin _ _) = -ymin * (-ymin - 1) `div` 2 

simulate :: Area Int -> V2 Int -> Bool
simulate area@(Area _ ymin _ _) (V2 vx vy) = 
    any ((`elem` area) . fst) . takeWhile ((>=ymin) . _y . fst) $ run where
    run = iterate' step (V2 0 0, V2 vx vy)
    step (p, dxy@(V2 dx dy)) = (p+dxy, V2 (max (dx-1) 0) (dy-1))

part2 :: Area Int -> Int
part2 area@(Area _ ymin xmax _) = count (simulate area) (V2 <$> [1..xmax] <*> [ymin..(-ymin)])

solve :: Text -> IO ()
solve = aoc parser part1 part2
