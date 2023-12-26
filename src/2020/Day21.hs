-- https://adventofcode.com/2020/day/21
module Day21 (solve) where
import           AOC.Prelude
import qualified Data.Text as Text
import qualified Data.HashMap.Strict as Map
import           AOC (aoc)
import           AOC.Parser (Parser, sepBy1, sepEndBy1, some, lowerChar, eol, lowerChar, hspace)
import           AOC.List (count)
import           AOC.Graph (perfectMatchings)
import qualified AOC.HashSet as Set

type Food = ([Text], [Text])

parser :: Parser [Food]
parser = food `sepEndBy1` eol where
    food = (,) <$> ingredients <*> allergens
    ingredients = name `sepEndBy1` hspace
    allergens = "(contains " *> name `sepBy1` ", " <* ")"
    name = Text.pack <$> some lowerChar

getMatching  :: [Food] -> Maybe [(Text, Text)]
getMatching foods = listToMaybe $ perfectMatchings graph where
    graph = Map.toList
            . Map.map Set.toList
            . Map.fromListWith Set.intersection 
            $ [(a, Set.fromList ingredients) | (ingredients, allergens) <- foods, a <- allergens]

part1 :: [Food] -> Maybe Int
part1 foods = do
    matching <- getMatching foods
    let badIngredients = Set.fromList (map snd matching)
    let allIngredients = concatMap fst foods
    Just $! count (`Set.notMember` badIngredients) allIngredients

part2 :: [Food] -> Maybe Text
part2 foods = do
    matching <- getMatching foods
    Just $! Text.intercalate "," . map snd $ sortOn fst matching

solve :: Text -> IO ()
solve = aoc parser part1 part2
