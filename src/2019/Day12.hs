-- https://adventofcode.com/2019/day/12
module Day12 (solve) where
import           AOC.Prelude hiding (last)
import           AOC (aoc)
import           AOC.Parser (Parser, eol, signedDecimal, sepEndBy1, scanf)
import           AOC.V3(V3(..), origin, manhattan, _x, _y, _z)
import           AOC.Util (times)
import           Data.Maybe (fromJust)
import           Lens.Micro.Extras (view)

data Moon = Moon !(V3 Int) !(V3 Int) deriving (Eq)

parser :: Parser [V3 Int]
parser = [scanf|$V3 <x={d}, y={d}, z={d}>|] `sepEndBy1` eol where
    d = signedDecimal

update :: [Moon] -> Moon -> Moon
update moons (Moon pos vel) = Moon (pos + vel') vel' where
    vel' = vel + sum [fmap signum (pos' - pos) | Moon pos' _ <- moons]

step :: [Moon] -> [Moon]
step moons = map (update moons) moons  

totalEnergy :: [Moon] -> Int
totalEnergy moons = sum [manhattan origin pos * manhattan origin vel | Moon pos vel <- moons] 

part1 :: [V3 Int] -> Int
part1 = totalEnergy . times 1000 step . map (`Moon` origin)

findRepetition :: Eq a => [a] -> Int
findRepetition [] = error "findRepetition: empty list"
findRepetition (x:xs) = 1 + fromJust (elemIndex x xs)

findCycleFor :: (V3 Int -> Int) -> [[Moon]] -> Int
findCycleFor f = findRepetition . map (map \(Moon pos vel) -> (f pos, f vel))

part2 :: [V3 Int] -> Int
part2 moons = lcm n1 (lcm n2 n3) where
    simulation = iterate' step $ map (`Moon` origin) moons
    n1 = findCycleFor (view _x) simulation
    n2 = findCycleFor (view _y) simulation
    n3 = findCycleFor (view _z) simulation

solve :: Text -> IO ()
solve = aoc parser part1 part2