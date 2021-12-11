module Day10 (solve) where
import Data.List (foldl')
import Data.Either (lefts, rights)
import Util (median)

parseLine :: String -> Either Char [Char]
parseLine = go [] where
    go stack [] = Right stack
    go stack  (x:xs) | x `elem` "([{<"                        = go (x:stack) xs
    go (s:ss) (x:xs) | [s, x] `elem` ["()", "[]", "{}", "<>"] = go ss xs
    go _ (x:_) = Left x

part1 :: [String] -> Int
part1 = sum . map weight . lefts . map parseLine where
        weight ')' = 3
        weight ']' = 57
        weight '}' = 1197
        weight '>' = 25137

part2 :: [String] -> Int 
part2 = median . map stackWeight . rights . map parseLine where
        weight '(' = 1
        weight '[' = 2
        weight '{' = 3
        weight '<' = 4
        stackWeight = foldl' (\acc x -> acc * 5 + weight x) 0

solve :: String -> Maybe (Int, Int)
solve s = let ls = lines s in
            pure (part1 ls, part2 ls)