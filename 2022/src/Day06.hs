-- https://adventofcode.com/2022/day/6
module Day06 (solve) where
import           RIO hiding (some)
import           RIO.List (findIndex)
import           Text.Megaparsec (anySingle, some)
import           Data.List.Split (divvy)
import           Util (Parser, aoc, allUnique)

parser :: Parser String
parser = some anySingle

solve' :: Int -> String -> Maybe Int
solve' n = fmap (+n) . findIndex allUnique . divvy n 1

solve :: MonadIO m => Text -> m ()
solve = aoc parser (solve' 4) (solve' 14)
