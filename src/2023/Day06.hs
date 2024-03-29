-- https://adventofcode.com/2023/day/6
module Day06 (solve) where
import           AOC.Prelude
import           Prelude (read)
import           AOC (aoc)
import           AOC.Number (toDouble)
import           AOC.Parser (Parser, sepEndBy1, eol, hspace, decimal)

type Input = ([Int], [Int]) -- time limits, distances

parser :: Parser Input
parser = (,) <$> ("Time:" *> list) <*> (eol *> "Distance:" *> list) where
    list = hspace *> decimal `sepEndBy1` hspace

solveFor :: (Input -> [(Int, Int)]) -> Input -> Int
solveFor toRaces = product . map solveForRace . toRaces where
    solveForRace (time, distance) = max 0 (ceiling root2 - floor root1 - 1) where
        t = toDouble time
        d = toDouble distance
        delta = sqrt $ max 0 (t*t - 4*d)
        root1 = (t - delta) / 2
        root2 = (t + delta) / 2

toOneRace :: Input -> [(Int, Int)]
toOneRace (l1, l2) = [(f l1, f l2)] where
    f = read . concatMap show

solve :: Text -> IO ()
solve = aoc parser (solveFor $ uncurry zip) (solveFor toOneRace)