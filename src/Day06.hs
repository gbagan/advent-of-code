module Day06 (solve) where
import Data.List.Split (splitOn)
import Text.Read (readMaybe)
import Data.List (iterate')
import Util (count)

data Vec = Vec Int Int Int Int Int Int Int Int Int

buildVec :: [Int] -> Vec
buildVec xs = Vec (f 0) (f 1) (f 2) (f 3) (f 4) (f 5) (f 6) (f 7) (f 8)
              where f i = count (==i) xs

algo :: Int -> Vec -> Int
algo n = vecSum . (!!n) . iterate' step where
            vecSum (Vec x0 x1 x2 x3 x4 x5 x6 x7 x8) = x0 + x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8          
            step (Vec x0 x1 x2 x3 x4 x5 x6 x7 x8) = Vec x1 x2 x3 x4 x5 x6 (x0+x7) x8 x0

solve :: String -> Maybe (Int, Int)
solve s = do
    vec <- buildVec <$> traverse readMaybe (splitOn "," s)
    pure (algo 80 vec, algo 256 vec)
