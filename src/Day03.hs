module Day03 (solve) where

import Data.Bool (bool)
import Data.List (foldl', partition, transpose)
import Util (digitToIntMaybe, majority)

data Bit = Zero | One deriving (Eq, Enum)

digitToBit :: Char -> Maybe Bit
digitToBit '0' = Just Zero
digitToBit '1' = Just One
digitToBit _ = Nothing

toDec :: [Bit] -> Int
toDec = foldl' (\acc x -> acc * 2 + fromEnum x) 0

part1 :: [[Bit]] -> Int
part1 l = toDec e * toDec g where
    g = map (bool Zero One . majority (==One)) (transpose l)
    e = map (bool One Zero . (==One)) g

data Common = MostCommon | LeastCommon deriving (Eq)

filterCode :: Common -> [[Bit]] -> [Bit]
filterCode common = go 0 where
    go _ [] = undefined
    go _ [x] = x
    go i xs =
        let (ys, zs) = partition (\x -> x !! i == One) xs in
        if (length ys >= length zs) == (common == MostCommon) then
            go (i + 1) ys
        else
            go (i + 1) zs

part2 :: [[Bit]] -> Int
part2 l = toDec x * toDec y where
    x = filterCode MostCommon l
    y = filterCode LeastCommon l

solve :: String -> Maybe (Int, Int)
solve s = do
    xs <- traverse (traverse digitToBit) (lines s)
    Just (part1 xs, part2 xs)
