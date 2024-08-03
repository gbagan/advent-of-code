-- https://adventofcode.com/2019/day/6
module Day06 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Parser (Parser, alphaNumChar, sepEndBy1, some, eol, scanf)
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import qualified Data.Text as Text

parser :: Parser (HashMap Text Text)
parser = Map.fromList . map swap <$> orbit `sepEndBy1` eol where
    object = Text.pack <$> some alphaNumChar
    orbit = [scanf|{object}){object}|]

pathToRoot :: HashMap Text Text -> Text -> [Text]
pathToRoot _ "COM" = []
pathToRoot orbits x = x : pathToRoot orbits (orbits Map.! x)

part1 :: HashMap Text Text -> Int
part1 orbits = sum . map (length . pathToRoot orbits) $ Map.keys orbits

part2 :: HashMap Text Text -> Int
part2 orbits = length path1 + length path2 - 2 * inter - 2 where
    path1 = pathToRoot orbits "YOU"
    path2 = pathToRoot orbits "SAN"
    inter = Set.size $ Set.intersection (Set.fromList path1) (Set.fromList path2)

solve :: Text -> IO ()
solve = aoc parser part1 part2