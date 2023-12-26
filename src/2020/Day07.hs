-- https://adventofcode.com/2020/day/7
module Day07 (solve) where
import           AOC.Prelude
import qualified Data.Text as Text
import qualified Data.HashSet as Set
import qualified Data.HashMap.Strict as Map
import           AOC (aoc)
import           AOC.Parser (Parser, optional, sepBy1, sepEndBy1, some, char, lowerChar, eol, decimal)

type Input = (Text, [(Int, Text)])

parser :: Parser [Input]
parser = input `sepEndBy1` eol where
    input = (,) <$> bag <* " contain " <*> 
        ([] <$ "no other bags"  <|> (bags `sepBy1` ", ")) <* char '.' 
    bag = Text.pack <$> ((++) <$> some lowerChar <* " " <*> some lowerChar <* " bag" <* optional "s")
    bags = (,) <$> decimal <* char ' ' <*> bag 

part1 :: [Input] -> Int
part1 is = Set.size (go "shinygold") - 1 where
    reverseMap = Map.fromListWith (++) [(y, [x]) | (x, ys) <- is, (_, y) <- ys]
    go name = Set.insert name $ Set.unions (map go (Map.lookupDefault [] name reverseMap))

part2 :: [Input] -> Int
part2 is = go "shinygold" - 1 where
    go bag = 1 + sum [i * go bag' | (i, bag') <- Map.lookupDefault [] bag m]
    m = Map.fromList is

solve :: Text -> IO ()
solve = aoc parser part1 part2