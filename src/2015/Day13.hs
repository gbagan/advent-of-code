-- https://adventofcode.com/2015/day/1
module Day13 (solve) where
import           AOC.Prelude
import           Data.List (minimum, maximum)
import           AOC (aoc')
import           AOC.Parser (Parser, decimal, some, eol, letterChar, sepEndBy1, format)
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as Text
import           AOC.Tuple (fst3, snd3)

type Network = HashMap (Text, Text) Int
type Input = ([Text], Network)

parser :: Parser [(Text, Text, Int)]
parser = row `sepEndBy1` eol where
    row = do
        (name1, s, happiness, name2) <- 
            [format|{name} would {sign} {decimal} happiness units by sitting next to {name}.|]
        pure (name1, name2, s * happiness)
    name = Text.pack <$> some letterChar
    sign = 1 <$ "gain" <|> (-1) <$ "lose"

precomp :: [(Text, Text, Int)] -> Input
precomp rows = (names, network) where
    names = ordNub (map fst3 rows ++ map snd3 rows) 
    network = Map.fromListWith (+) 
                $ concatMap (\(n1, n2, h) -> [((n1, n2), h), ((n2, n1), h)])
                rows

solveFor :: ([Int] -> Int) -> Input -> Int
solveFor happinessFunc (name:names, network) = maximum 
                                            . map (happiness name) 
                                            $ permutations names
    where happiness x xs = happinessFunc [ network Map.! (y, z) 
                                         | (y, z) <- zip (x:xs) (xs++[x])
                                         ]
solveFor _ _ = error "solveFor: empty name list"

happinessFunc1, happinessFunc2 :: [Int] -> Int
happinessFunc1 = sum
happinessFunc2 xs = sum xs - minimum xs

solve :: Text -> IO ()
solve = aoc' parser (pure . precomp) (solveFor happinessFunc1) (solveFor happinessFunc2)
