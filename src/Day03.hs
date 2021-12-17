module Day03 (solve) where
import           Data.Functor (($>))
import           Data.List (partition, transpose)
import           Text.Megaparsec (sepEndBy1, some, (<|>))
import qualified Text.Megaparsec.Char as P
import           Util (Parser, aocTemplate, binToInt, majority)

parser :: Parser [[Bool]]
parser = some bit `sepEndBy1` P.eol where
    bit = P.char '0' $> False <|> P.char '1' $> True

part1 :: [[Bool]] -> Int
part1 l = binToInt e * binToInt g where
    g = map (majority id) (transpose l)
    e = map not g

data Common = MostCommon | LeastCommon deriving (Eq)

filterCode :: Common -> [[Bool]] -> [Bool]
filterCode common = go 0 where
    go _ [] = []
    go _ [x] = x
    go i xs =
        let (ys, zs) = partition (!!i) xs in
        if (length ys >= length zs) == (common == MostCommon) then
            go (i + 1) ys
        else
            go (i + 1) zs

part2 :: [[Bool]] -> Int
part2 l = binToInt x * binToInt y where
    x = filterCode MostCommon l
    y = filterCode LeastCommon l

solve :: String -> IO ()
solve = aocTemplate parser (pure . part1) (pure . part2)