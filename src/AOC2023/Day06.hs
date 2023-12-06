-- https://adventofcode.com/2023/day/6
module AOC2023.Day06 (solve) where
import           RIO
import           RIO.Partial (read)
import           Text.Megaparsec (sepEndBy1)
import           Text.Megaparsec.Char (eol, hspace)
import           Text.Megaparsec.Char.Lexer (decimal)
import           Util (Parser, aoc)

type Input = ([Int], [Int])

parser :: Parser Input
parser = (,) <$> ("Time:" *> list) <*> (eol *> "Distance:" *> list) where
    list = hspace *> decimal `sepEndBy1` hspace

solveWith :: (Input -> [(Int, Int)]) -> Input -> Int
solveWith toRaces = product . map forRace . toRaces where
    forRace (time, distance) = ceiling root2 - floor root1 - 1 where
        t = fromIntegral time :: Double
        d = fromIntegral distance :: Double
        delta = sqrt (t*t - 4*d)
        root1 = (t - delta) / 2
        root2 = (t + delta) / 2

toOneRace :: Input -> [(Int, Int)]
toOneRace (l1, l2) = [(f l1, f l2)] where
    f = read . concatMap show

solve :: MonadIO m => Text -> m ()
solve = aoc parser (solveWith $ uncurry zip) (solveWith toOneRace)