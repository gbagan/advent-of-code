-- https://adventofcode.com/2021/day/2
module Day02 (solve) where

import Data.List (foldl')
import Text.Read (readMaybe)

data Instr = Forward Int | Down Int

part1 :: [Instr] -> Int
part1 l = tx * ty where
        (tx, ty) =
            foldl' (\(x, y) -> \case
                Forward v -> (x + v, y)
                Down v    -> (x, y + v)
            ) (0, 0) l

part2 :: [Instr] -> Int
part2 l = tx * ty where
        (tx, ty, _) =
            foldl' (\(x, y, aim) -> \case
                Forward v -> (x + v, y + aim * v, aim)
                Down v    -> (x, y, aim + v)
            ) (0, 0, 0) l

parseLine :: [String] -> Maybe Instr
parseLine [x, y] = do
    v <- readMaybe y
    case x of
        "forward" -> Just (Forward v)
        "down"    -> Just (Down v)
        "up"      -> Just (Down (-v))
        _         -> Nothing
parseLine _ = Nothing

solve :: String -> Maybe (Int, Int)
solve s = do
    l <- traverse (parseLine . words) (lines s)
    Just (part1 l, part2 l)
