-- https://adventofcode.com/2018/day/1
module Day01 (solve) where
import           AOC.Prelude
import qualified Data.HashSet as Set
import           AOC (aoc)
import           AOC.Parser (Parser, sepEndBy1, eol, decimal)

parser :: Parser [Int]
parser = signedDecimal `sepEndBy1` eol where
    signedDecimal = (id <$ "+" <|> negate <$ "-") <*> decimal

findRepetition :: Hashable a => [a] -> Maybe a
findRepetition = go Set.empty where
    go _ [] = Nothing
    go seen (x : xs) | x `Set.member` seen = Just x
                     | otherwise = go (Set.insert x seen) xs

part1 :: [Int] -> Int
part1 = sum

part2 :: [Int] -> Maybe Int
part2 = findRepetition . scanl' (+) 0 . cycle

solve :: Text -> IO ()
solve = aoc parser part1 part2