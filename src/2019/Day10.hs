module Day10 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Parser (Parser, some, sepEndBy1, eol)
import           AOC.V2 (V2(..), manhattan)
import qualified AOC.HashSet as Set
import           AOC.List (count, groupOn, maximumOn)
import           AOC.Util (listTo2dSet)
import           AOC.Number (toDouble)
import           Data.List ((!!))

parser :: Parser (HashSet (V2 Int))
parser = listTo2dSet <$> some asteroid `sepEndBy1` eol where
    asteroid = True <$ "#" <|> False <$ "."

between :: V2 Int -> V2 Int -> [V2 Int]
between pos1 pos2 = [pos1 + fmap (*i) dxy | i <- [1..g-1]] where
    V2 dx dy = pos2 - pos1
    g = gcd dx dy
    dxy = V2 (dx `div` g) (dy `div` g)

canDetect :: HashSet (V2 Int) -> V2 Int -> V2 Int -> Bool
canDetect asteroids pos1 pos2 = and [p `Set.notMember` asteroids | p <- between pos1 pos2]

findStation :: HashSet (V2 Int) -> (V2 Int, Int)
findStation asteroids = maximumOn snd [(pos, count (canDetect asteroids pos) asteroids - 1) | pos <- Set.toList asteroids]

part1 :: HashSet (V2 Int) -> Int
part1 = snd . findStation

angle :: V2 Int -> V2 Int -> Double
angle station (V2 y x) = atan2 (-toDouble dx) (toDouble dy) where
    V2 dy dx = V2 y x - station

part2 :: HashSet (V2 Int) -> Int
part2 asteroids = x * 100 + y where
    station = fst $ findStation asteroids
    vaporized = concat
        . transpose
        . map (sortOn (manhattan station))
        . groupOn (angle station) 
        . sortOn (angle station)
        . Set.toList 
        $ Set.delete station asteroids
    V2 y x = vaporized !! 199

solve :: Text -> IO ()
solve = aoc parser part1 part2