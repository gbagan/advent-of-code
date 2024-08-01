-- https://adventofcode.com/2018/day/8
module Day08 (solve) where
import           AOC.Prelude
import           AOC (aoc')
import           AOC.Parser (Parser, anySingle, count, sepEndBy1, hspace, decimal)
import           Text.Megaparsec (Parsec, parseMaybe)

data Tree = Tree [Tree] [Int]

parser :: Parser [Int]
parser = decimal `sepEndBy1` hspace

treeParser :: Parsec Void [Int] Tree
treeParser = do
    nodeLen <- anySingle
    metadataLen <- anySingle     
    children <- count nodeLen treeParser
    metadata <- count metadataLen anySingle
    pure (Tree children metadata) 

part1 :: Tree -> Int
part1 (Tree children metadata) = sum (map part1 children) + sum metadata

part2 :: Tree -> Int
part2 (Tree [] metadata) = sum metadata
part2 (Tree children metadata) = sum [part2 <$> children !!? (idx-1) ?: 0 | idx <- metadata]

solve :: Text -> IO ()
solve = aoc' parser (parseMaybe treeParser) part1 part2