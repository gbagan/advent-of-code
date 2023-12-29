-- https://adventofcode.com/2016/day/11
module Day11 (solve) where
import           AOC.Prelude hiding (State, state)
import           AOC (aoc)
import           AOC.Parser (Parser, eol, sepEndBy1, sepBy1, some, letterChar, format)
import qualified Data.Text as Text
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Instances ()
import           AOC.List (grouped2, pairwise)
import           AOC.Graph (distance)

data ItemType = Generator | Microchip deriving (Eq, Ord, Show)
data Item = Item !Text !ItemType  deriving (Eq, Ord, Show)

data State = State Int (V.Vector Int) deriving (Eq, Show)
    -- elevator floor, pairs of generator, microchip positions

instance Hashable State where
    hashWithSalt s (State x xs) = s `hashWithSalt` x `hashWithSalt` xs

subsets2 :: [a] -> [[a]]
subsets2 xs = [[]] ++ map pure xs ++ pairwise (\x y -> [x, y]) xs

parser :: Parser [[Item]]
parser = row `sepEndBy1` eol where
    row = snd <$> [format|The {nth} floor contains {components}.|]
    nth = "first" <|> "second" <|> "third" <|> "fourth"
    components = [] <$"nothing relevant"
                <|> component `sepBy1` (", and " <|> ", ")
    component = Item <$> ("a " *> label) <*> itemtype
    itemtype = Generator <$ " generator" <|> Microchip <$ "-compatible microchip"
    label = Text.pack <$> some letterChar

isValid :: State -> Bool
isValid (State _ items) = all isValid' indexed where
    isValid' (idx, (gen, chip)) = 0 <= gen && gen <= 3 && 0 <= chip && chip <= 3
                        && (gen == chip
                            || all (\(idx', (gen', _)) -> idx == idx' || chip /= gen') indexed
                           )
    indexed = zip [(0::Int)..] . grouped2 $ V.toList items

atSameFloor :: State -> [Int]
atSameFloor (State elevator items) = [ i | (i, f) <- zip [0..] (V.toList items), f==elevator]

neighbors :: State -> [State]
neighbors state@(State elevator items) = states where
    states =
        [ state' 
        | direction <- [1, - 1]
        , let elevator' = elevator + direction
        , c <- subsets2 (atSameFloor state)
        , let items' = items V.// map (, elevator') c
        , let state' = State elevator' items'
        , isValid state'
        ]

startingState :: [[Item]] -> State
startingState items = State 0 items'' where
    items'' = V.fromList . map snd $ sortOn fst [ (item, i)
                                                | (i, items') <- zip [0..] items
                                                , item <- items'
                                                ]

isDest :: State -> Bool
isDest (State _ items) = all (==3) (V.toList items)

part1 :: [[Item]] -> Int
part1 items = traceShow (startingState items) 0

part2 :: [[Item]] -> Maybe Int
part2 items = distance neighbors isDest (startingState items)

solve :: Text -> IO ()
solve = aoc parser part1 part2