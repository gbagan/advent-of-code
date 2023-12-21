module AOC2021.Day06 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Parser (Parser, char, decimal, sepBy1)
import           AOC.List (count)
import           AOC.Util (times)

data Vec = Vec !Int !Int !Int !Int !Int !Int !Int !Int !Int

parser :: Parser [Int]
parser = decimal `sepBy1` char ','

buildVec :: [Int] -> Vec
buildVec xs = Vec (f 0) (f 1) (f 2) (f 3) (f 4) (f 5) (f 6) (f 7) (f 8)
              where f i = count (==i) xs

solveFor :: Int -> [Int] -> Int
solveFor n = vecSum . times n step . buildVec where
            vecSum (Vec x0 x1 x2 x3 x4 x5 x6 x7 x8) = x0 + x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8          
            step (Vec x0 x1 x2 x3 x4 x5 x6 x7 x8) = Vec x1 x2 x3 x4 x5 x6 (x0+x7) x8 x0

solve :: Text -> IO ()
solve = aoc parser (solveFor 80) (solveFor 256)