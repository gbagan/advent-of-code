module Day14 (solve) where
import           AOC.Prelude hiding (head, last)
import           Data.List (head, last, minimum, maximum)
import qualified Data.Map.Strict as Map
import           AOC (aoc)
import           AOC.Util (times)
import           AOC.Parser (Parser, eol, sepEndBy1, some, upperChar)

type Rules = Map String Char
type PairsMap = Map (Char, Char) Int
data Input = Input String Rules

parser :: Parser Input
parser = Input <$> some upperChar <* eol <* eol <*> rules where
    rules = Map.fromList <$> sepEndBy1 rule eol
    rule = (,) <$> some upperChar <* " -> " <*> upperChar

stringToPairsMap :: String -> PairsMap
stringToPairsMap s = Map.fromListWith (+) $ zipWith (curry (,1)) s (drop 1 s)

step :: Rules -> PairsMap -> PairsMap
step rules = Map.fromListWith (+)
                . Map.foldlWithKey' (\acc (a, b) v ->
                    case Map.lookup [a, b] rules of
                        Nothing -> ((a, b), v) : acc
                        Just c  -> ((a, c), v) : ((c, b), v) : acc
                ) []

pairsMapToFreqs :: String -> PairsMap -> Map Char Int
pairsMapToFreqs str =
    Map.map (`div` 2)
    . Map.adjust (+1) (last str)
    . Map.adjust (+1) (head str)
    . Map.fromListWith (+)
    . Map.foldlWithKey' (\acc (a, b) v -> (a, v) : (b, v) : acc) []

solveFor :: Int -> Input -> Int
solveFor n (Input s rules) = maximum freqs - minimum freqs where
    pairs = stringToPairsMap s
    freqs = Map.elems . pairsMapToFreqs s $ times n (step rules) pairs

solve :: Text -> IO ()
solve = aoc parser (solveFor 10) (solveFor 40)