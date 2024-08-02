-- https://adventofcode.com/2018/day/10
module Day10 (solve) where
import           AOC.Prelude
import           AOC (aoc')
import           AOC.Parser (Parser, sepEndBy1, eol, hspace, signedDecimal, scanf)
import           AOC.V2 (V2(..))
import           AOC.Area (Area(..), boundingBox)
import           AOC.Draw (drawPoints)

parser :: Parser [(V2 Int, V2 Int)]
parser = [scanf|position=<{vec}> velocity=<{vec}>|] `sepEndBy1` eol where
    vec = V2 <$> (hspace *> signedDecimal) <*> ("," *> hspace *> signedDecimal)

step :: [(V2 Int, V2 Int)] -> [(V2 Int, V2 Int)]
step = map \(pos, dir) -> (pos+dir, dir) 

isNice :: [V2 Int] -> Bool
isNice positions = ymax - ymin <= 10 where
    Area _ ymin _ ymax = boundingBox positions

findNice :: [(V2 Int, V2 Int)] -> Maybe (Int, [V2 Int])
findNice = find (isNice . snd) . zip [0..] . map (map fst) . iterate' step

part1 :: (a, [V2 Int]) -> Int
part1 (_, points) = trace (drawPoints points) 0

part2 :: (Int, a) -> Int
part2 = fst

solve :: Text -> IO ()
solve = aoc' parser findNice part1 part2