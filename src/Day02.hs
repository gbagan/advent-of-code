-- https://adventofcode.com/2021/day/2
module Day02 (solve) where

part1 :: [(String, Int)] -> Int
part1 l = 
    let (tx, ty) =
            foldl (\(x, y) (instr, v) -> case instr of
                "forward" -> (x + v, y)
                "down"    -> (x, y + v)
                "up"      -> (x, y - v)
            ) (0, 0) l
    in tx * ty

part2 :: [(String, Int)] -> Int
part2 l = 
    let (tx, ty, _) =
            foldl (\(x, y, aim) (instr, v) -> case instr of
                "forward" -> (x + v, y + aim * v, aim)
                "down"    -> (x, y, aim + v)
                "up"      -> (x, y, aim - v)
            ) (0, 0, 0) l
    in tx * ty

f :: [String] -> (String, Int)
f [x, y] = (x, read y)

solve :: String -> Maybe (Int, Int)
solve s = 
    let l = map (f . words) . lines $ s in
    Just (part1 l, part2 l)
