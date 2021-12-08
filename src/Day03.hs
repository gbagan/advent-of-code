module Day03 (solve) where
import Data.Char (digitToInt)
import Data.List (foldl', partition, transpose)
import Util (majority)

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

gammaRate :: [String] -> String
gammaRate = map go where 
                go s = if majority (=='1') s then '1' else '0'

part1 :: [String] -> Int
part1 l = toDec e * toDec g where
    g = gammaRate (transpose l)
    e = map (\x -> if x == '0' then '1' else '0') g

data Common = MostCommon | LeastCommon deriving (Eq)

filterCode :: Common -> [String] -> String
filterCode common = go 0 where
    go _ [] = undefined
    go _ [x] = x
    go i xs =
        let (ys, zs) = partition (\x -> x !! i == '1') xs in
        if (length ys >= length zs) == (common == MostCommon) then
            go (i + 1) ys
        else
            go (i + 1) zs

part2 :: [String] -> Int
part2 l = toDec x * toDec y where
    x = filterCode MostCommon l
    y = filterCode LeastCommon l

solve :: String -> Maybe (Int, Int)
solve s =
    let xs = lines s in
    Just (part1 xs, part2 xs)
