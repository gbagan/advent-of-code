{-# LANGUAGE BlockArguments #-}

module Main where
import Data.Char (digitToInt)
import Data.List (foldl', partition, (!!))

gammaRate :: [String] -> String
gammaRate l = map (\v -> if v >= ndiv2 then '1' else '0')
            . foldl' (zipWith \x y -> x + digitToInt y) (repeat 0)
            $ l
        where ndiv2 = length l `div` 2

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

algo1 :: [String] -> Int
algo1 l = toDec e * toDec g where
    g = gammaRate l
    e = map (\x -> if x == '0' then '1' else '0') g

data Common = MostCommon | LeastCommon deriving (Eq)

filterCode :: Common -> [String] -> String
filterCode common l = go 0 l where
    go _ [] = undefined
    go _ [x] = x
    go i xs =
        let (ys, zs) = partition (\x -> x !! i == '1') xs in
        if (length ys >= length zs) == (common == MostCommon) then
            go (i + 1) ys
        else
            go (i + 1) zs

algo2 :: [String] -> Int
algo2 l = toDec x * toDec y where
    x = filterCode MostCommon l
    y = filterCode LeastCommon l

main = do
    xs <- lines <$> readFile "data03"
    print $ algo1 xs
    print $ algo2 xs
