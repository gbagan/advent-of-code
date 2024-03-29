module Day03 (solve) where
import           AOC.Prelude
import           Data.List ((!!))
import           AOC (aoc)
import           AOC.Parser (Parser, bitP, eol, sepEndBy1, some)
import           AOC.Util (binToInt, majority)

parser :: Parser [[Bool]]
parser = some bitP `sepEndBy1` eol

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

solve :: Text -> IO ()
solve = aoc parser part1 part2