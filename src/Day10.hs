module Day10 where
import Data.List (foldl')
import Data.Maybe (mapMaybe)
import Util (median)

weight :: Char -> Int
weight ')' = 3
weight ']' = 57
weight '}' = 1197
weight '>' = 25137

part1 :: [String] -> Int 
part1 = sum . map (go []) where
    go _ [] = 0
    go stack  (x:xs) | x `elem` "([{<"                        = go (x:stack) xs
    go (s:ss) (x:xs) | [s, x] `elem` ["()", "[]", "{}", "<>"] = go ss xs
    go _ (x:xs) = weight x

weight2 :: Char -> Int
weight2 '(' = 1
weight2 '[' = 2
weight2 '{' = 3
weight2 '<' = 4

stackWeight :: [Char] -> Int 
stackWeight = foldl' (\acc x -> acc * 5 + weight2 x) 0

part2 :: [String] -> Int 
part2 = median . mapMaybe (go []) where
    go stack [] = Just $ stackWeight stack
    go stack  (x:xs) | x `elem` "([{<"                        = go (x:stack) xs
    go (s:ss) (x:xs) | [s, x] `elem` ["()", "[]", "{}", "<>"] = go ss xs
    go _ (x:xs) = Nothing

solve :: String -> Maybe (Int, Int)
solve s =
    let ls = lines s in
    pure (part1 ls, part2 ls)