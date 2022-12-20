module AOC2020.Day10 (solve) where
import           RIO
import           RIO.List (sort)
import           RIO.List.Partial (maximum)
import qualified RIO.Vector as V
import qualified RIO.Vector.Partial as V ((!))
import qualified Data.IntSet as Set
import           Text.Megaparsec (sepEndBy1)
import           Text.Megaparsec.Char (eol)
import           Text.Megaparsec.Char.Lexer (decimal)
import           Util (Parser, aoc, count)

parser :: Parser [Int]
parser = decimal `sepEndBy1` eol

part1 :: [Int] -> Int
part1 l = count (\(a, b) -> b - a == 1) sl * (1 + count (\(a, b) -> b - a == 3) sl) where
    l' = sort l
    sl = zip (0 : l') l'

part2 :: [Int] -> Integer
part2 l = memo V.! n where
    memo :: Vector Integer
    memo = V.generate (n+1) go
    go i
        | i == 0 = 1
        | i `Set.member` s = sum [fromMaybe 0 (memo V.!? (i - j)) | j<- [1..3]]
        | otherwise = 0
    s = Set.fromList (0 : n : l)
    n = maximum l + 3

solve :: MonadIO m => Text -> m ()
solve = aoc parser part1 part2