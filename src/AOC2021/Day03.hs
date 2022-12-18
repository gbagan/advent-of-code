module AOC2021.Day03 (solve) where
import           RIO hiding (some)
import           RIO.List (partition, transpose)
import           RIO.List.Partial ((!!))
import           Text.Megaparsec (sepEndBy1, some)
import           Text.Megaparsec.Char (eol)
import           Util (Parser, aoc, binToInt, bitP, majority)

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

solve :: MonadIO m => Text -> m ()
solve = aoc parser part1 part2