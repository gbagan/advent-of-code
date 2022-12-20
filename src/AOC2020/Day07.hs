-- https://adventofcode.com/2020/day/7
module AOC2020.Day07 (solve) where
import           RIO hiding (optional, some)
import qualified RIO.Text as Text
import qualified RIO.HashSet as Set
import qualified RIO.HashMap as Map
import           Text.Megaparsec (optional, sepBy1, sepEndBy1, some)
import           Text.Megaparsec.Char (char, lowerChar, eol, string)
import           Text.Megaparsec.Char.Lexer (decimal)
import           Util (Parser, aoc)

type Input = (Text, [(Int, Text)])

parser :: Parser [Input]
parser = input `sepEndBy1` eol where
    input = (,) <$> bag <* string " contain " <*> 
        ([] <$ "no other bags"  <|> (bags `sepBy1` string ", ")) <* char '.' 
    bag = Text.pack <$> ((++) <$> some lowerChar <* char ' ' <*> some lowerChar <* string " bag" <* optional (char 's'))
    bags = (,) <$> decimal <* char ' ' <*> bag 

part1 :: [Input] -> Int
part1 is = Set.size (go "shinygold") - 1 where
    reverseMap = Map.fromListWith (++) [(y, [x]) | (x, ys) <- is, (_, y) <- ys] 
    go name = Set.insert name $ Set.unions (map go (Map.lookupDefault [] name reverseMap))

part2 :: [Input] -> Int
part2 is = go "shinygold" - 1 where
    go bag = 1 + sum [i * go bag' | (i, bag') <- Map.lookupDefault [] bag m]
    m = Map.fromList is

solve :: MonadIO m => Text -> m ()
solve = aoc parser part1 part2