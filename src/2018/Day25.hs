-- https://adventofcode.com/2018/day/25
module Day25 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Parser (Parser, sepEndBy1, eol, signedDecimal, scanf)
import           AOC.V4 (V4(..), manhattan)
import           AOC.Graph (connectedComponents, fromEdgePredicate)

parser :: Parser [V4 Int]
parser = v4 `sepEndBy1` eol where
    v4 = [scanf|$V4 {d},{d},{d},{d}|]
    d = signedDecimal

part1 :: [V4 Int] -> Int
part1 = length . connectedComponents . flip fromEdgePredicate p where
    p v1 v2 = manhattan v1 v2 <= 3

solve :: Text -> IO ()
solve = aoc parser part1 (pure ())