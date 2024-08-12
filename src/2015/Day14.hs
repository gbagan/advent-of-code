-- https://adventofcode.com/2015/day/14
module Day14 (solve) where
import           AOC.Prelude hiding (max)
import           AOC (aoc)
import           AOC.List (maximum, drop1)
import           AOC.Parser (Parser, decimal, eol, letterChar, some, sepEndBy1, scanf)

type Reindeer = (Int, Int, Int) -- speed, duration, rest time

parser :: Parser [Reindeer]
parser = row `sepEndBy1` eol where
    name = some letterChar
    row = do
        (_, speed, duration, rest) <-
            [scanf|{name} can fly {decimal} km/s for {decimal} seconds, but then must rest for {decimal} seconds.|]
        pure (speed, duration, rest)

distance :: Int -> Reindeer -> Int
distance totalDuration (speed, duration, rest) =
    let (q, r) = totalDuration `quotRem` (duration + rest) in
    q * duration * speed + min r duration * speed

part1 :: [(Int, Int, Int)] -> Int
part1  = maximum . map (distance 2503)

positions :: Reindeer -> [Int]
positions (speed, duration, rest) = 
    take 2503
    . drop1
    . scanl' (+) 0 
    . cycle $ 
        replicate duration speed ++ replicate rest 0

award :: [Int] -> [Int]
award ps = [if pos == max then 1 else 0 | pos <- ps]
    where max = maximum ps

part2 :: [(Int, Int, Int)] -> Int
part2 = maximum
    . map sum
    . transpose
    . map award
    . transpose
    . map positions

solve :: Text -> IO ()
solve = aoc parser part1 part2
