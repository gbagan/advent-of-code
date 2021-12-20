module Day14 (solve) where
import           Data.Map (Map)
import qualified Data.Map as Map
import           Text.Megaparsec (sepEndBy1, some)
import qualified Text.Megaparsec.Char as P
import           Util (Parser, aocTemplate)

type Rules = Map String Char
type PairsMap = Map (Char, Char) Int
data Input = Input String Rules

parser :: Parser Input
parser = Input <$> some P.upperChar <* P.eol <* P.eol <*> rules where
    rules = Map.fromList <$> sepEndBy1 rule P.eol
    rule = (,) <$> some P.upperChar <* P.string " -> " <*> P.upperChar

stringToPairsMap :: String -> PairsMap
stringToPairsMap s = Map.fromListWith (+) $ zip (zip s (tail s)) (repeat 1) 

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

algo :: Int -> Input -> Int
algo n (Input s rules) = maximum freqs - minimum freqs where
    pairs = stringToPairsMap s
    freqs = Map.elems . pairsMapToFreqs s $ iterate (step rules) pairs !! n

solve :: String -> IO ()
solve = aocTemplate parser pure (pure . algo 10) (pure . algo 40)