-- https://adventofcode.com/2021/day/1
module Main where

algo1 :: [(String, Int)] -> Int
algo1 l = 
    let (tx, ty) =
            foldl (\(x, y) (dir, v) -> case dir of
                "forward" -> (x + v, y)
                "down"    -> (x, y + v)
                "up"      -> (x, y - v)
            ) (0, 0) l
    in tx * ty

algo2 :: [(String, Int)] -> Int
algo2 l = 
    let (tx, ty, _) =
            foldl (\(x, y, aim) (dir, v) -> case dir of
                "forward" -> (x + v, y + aim * v, aim)
                "down"    -> (x, y, aim + v)
                "up"      -> (x, y, aim - v)
            ) (0, 0, 0) l
    in tx * ty

f :: [String] -> (String, Int)
f [x, y] = (x, read y)

main = do
    l <- map (f . words) . lines <$> readFile "data02"
    print $ algo1 l
    print $ algo2 l
