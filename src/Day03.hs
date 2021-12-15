module Day03 (solve) where
import           Data.Bool (bool)
import           Data.Functor (($>))
import           Data.List (foldl', partition, transpose)
import           Text.Megaparsec (sepEndBy1, some, (<|>))
import qualified Text.Megaparsec.Char as P
import           Util (Parser, aocTemplate, majority)

data Bit = Zero | One deriving (Eq, Enum)

parser :: Parser [[Bit]]
parser = some bit `sepEndBy1` P.eol where
    bit = P.char '0' $> Zero <|> P.char '1' $> One

toDec :: [Bit] -> Int
toDec = foldl' (\acc x -> acc * 2 + fromEnum x) 0

part1 :: [[Bit]] -> Int
part1 l = toDec e * toDec g where
    g = map (bool Zero One . majority (==One)) (transpose l)
    e = map (bool One Zero . (==One)) g

data Common = MostCommon | LeastCommon deriving (Eq)

filterCode :: Common -> [[Bit]] -> [Bit]
filterCode common = go 0 where
    go _ [] = []
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

solve :: String -> IO ()
solve = aocTemplate parser (Just . part1) (Just . part2)
