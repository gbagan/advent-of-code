-- https://adventofcode.com/2015/day/1
module Day13 (solve) where
import           AOC.Prelude
import           AOC (aoc')
import           AOC.Parser (Parser, decimal, some, eol, hspace, letterChar, sepEndBy1)
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as Text
import           AOC.Tuple (fst3, snd3)

type Network = HashMap (Text, Text) Int
type Input = ([Text], Network)

parser :: Parser [(Text, Text, Int)]
parser = row `sepEndBy1` eol where
    row = do
        name1 <- name <* " would "
        sign <- (1 <$ "gain" <|> (-1) <$ "lose") <* hspace
        happiness <- decimal <* " happiness units by sitting next to "
        name2 <- name <* "."
        pure (name1, name2, sign * happiness)
    name = Text.pack <$> some letterChar

precomp :: [(Text, Text, Int)] -> Input
precomp rows = (names, network) where
    names = ordNub (map fst3 rows ++ map snd3 rows) 
    network = foldl' (flip go) Map.empty rows
    go (name1, name2, happiness) = Map.insert (name1, name2) happiness . Map.insert (name2, name1) happiness

part1 :: [Int] -> Int
part1 = sum

part2 :: [Int] -> Maybe Int
part2 = findIndex (<0) . scanl' (+) 0

solve :: Text -> IO ()
solve = aoc' parser precomp part1 part2
