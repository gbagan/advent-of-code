-- https://adventofcode.com/2015/day/16
module Day16 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import qualified Data.HashMap.Strict as Map
import           AOC.Parser (Parser, eol, letterChar, sepBy1, sepEndBy1,some, decimal, scanf)
import qualified Data.Text as Text

parser :: Parser [(Int, HashMap Text Int)]
parser = sue `sepEndBy1` eol where
    sue = [scanf|Sue {decimal}: {things}|]
    things = Map.fromList <$> thing `sepBy1` ", "
    thing = (,) <$> label <* ": " <*> decimal
    label = Text.pack <$> some letterChar

clues :: HashMap Text Int
clues = Map.fromList
    [ ("children", 3)
    , ("cats", 7)
    , ("samoyeds", 2)
    , ("pomeranians", 3)
    , ("akitas", 0)
    , ("vizslas", 0)
    , ("goldfish", 5)
    , ("trees", 3)
    , ("cars", 2)
    , ("perfumes", 1)
    ]

solveFor :: (Text -> Int -> Int -> Bool) -> [(Int, HashMap Text Int)] -> Maybe Int
solveFor f = fmap fst . find (and . Map.intersectionWithKey f clues . snd)

check1, check2 :: Text -> Int -> Int -> Bool
check1 _ = (==)
check2 "cats" = (<)
check2 "trees" = (<)
check2 "pomeranians" = (>)
check2 "goldfish" = (>)
check2 _ = (==)

solve :: Text -> IO ()
solve = aoc parser (solveFor check1) (solveFor check2)
