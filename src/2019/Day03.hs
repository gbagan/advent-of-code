-- https://adventofcode.com/2019/day/3
module Day03 (solve) where
import           AOC.Prelude
import           AOC (aoc')
import           AOC.Parser (Parser, decimal, sepEndBy1, eol)
import           AOC.List (minimumMaybe)
import           AOC.V2 (V2(..), origin, south, north, west, east, manhattan)
import qualified Data.HashMap.Strict as Map

type Wire = [(V2 Int, Int)]

parser :: Parser (Wire, Wire)
parser = (,) <$> wire <* eol <*> wire where
    wire = ((,) <$> direction <*> decimal) `sepEndBy1` ","
    direction = south <$ "D" <|> north <$ "U" <|> west <$ "L" <|> east <$ "R"

wirePoints :: Wire -> HashMap (V2 Int) Int
wirePoints = 
    Map.fromListWith min
    . (`zip` [1..])
    . scanl1 (+)
    . concatMap (\(dir, n) -> replicate n dir)

wireIntersection ::  (Wire, Wire) -> HashMap (V2 Int) Int
wireIntersection (wire1, wire2) = Map.intersectionWith (+) points1 points2 where
    points1 = wirePoints wire1
    points2 = wirePoints wire2

part1 :: HashMap (V2 Int) Int -> Maybe Int
part1 = minimumMaybe . map (manhattan origin) . Map.keys

part2 :: HashMap (V2 Int) Int -> Maybe Int
part2 = minimumMaybe . Map.elems

solve :: Text -> IO ()
solve = aoc' parser (Just . wireIntersection) part1 part2
