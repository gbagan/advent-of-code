-- https://adventofcode.com/2019/day/14
module Day14 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Parser (Parser, decimal, eol, hspace, upperChar, sepBy1, sepEndBy1, some)
import           AOC.Graph (lexicographicTopologicalOrdering)
import           AOC.Util (maxBinSearch')
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as Text

type Input = HashMap Text (Int, [(Int, Text)])

parser :: Parser Input
parser = Map.fromList <$> reaction `sepEndBy1` eol where
    reaction = f <$> (chemical'  `sepBy1` ", ") <* " => " <*> chemical'
    chemical = Text.pack <$> some upperChar
    chemical' = (,) <$> decimal <* hspace <*> chemical
    f cs (n, c) = (c, (n, cs))

edges :: Input -> [(Text, Text)]
edges = concatMap (\(c, (_, cs)) -> map ((c,) . snd) cs) . Map.toList 

amountOfOreForFuel :: Input -> [Text] -> Int -> Int
amountOfOreForFuel  reactions ordering n = foldl' go (Map.singleton "FUEL" n) ordering Map.! "ORE" where
    go neededMap chemical = Map.unionWith (+) neededMap (Map.fromList toUpdate) where 
        (q, inputs) = Map.findWithDefault (0, []) chemical reactions
        needed = Map.findWithDefault 0 chemical neededMap 
        toUpdate = map (\(q', chemical') -> (chemical', q' * divUp needed q)) inputs

part1 :: Input -> Int
part1 reactions = amountOfOreForFuel reactions ordering 1 where
    ordering = lexicographicTopologicalOrdering (edges reactions)

part2 :: Input -> Int
part2 reactions = maxBinSearch' \n -> amountOfOreForFuel reactions ordering n <= 1_000_000_000_000 where
    ordering = lexicographicTopologicalOrdering (edges reactions)

divUp :: Int -> Int -> Int
divUp n m | n `mod` m == 0 = n `div` m
          | otherwise      = (n `div` m) + 1

solve :: Text -> IO ()
solve = aoc parser part1 part2