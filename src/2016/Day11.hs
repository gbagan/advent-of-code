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

data State = State Int (V.Vector Int) deriving (Eq, Ord, Show)
    -- elevator floor, pairs of generator, microchip positions

instance Hashable State where
    hashWithSalt s (State x xs) = s `hashWithSalt` x `hashWithSalt` xs

parser :: Parser [[Item]]
parser = row `sepEndBy1` eol where
    row = snd <$> [format|The {nth} floor contains {components}.|]
    nth = "first" <|> "second" <|> "third" <|> "fourth"
    components = [] <$ "nothing relevant"
                <|> component `sepBy1` (", and " <|> ", ")
    component = Item <$> ("a " *> label) <*> itemtype
    itemtype = Generator <$ " generator" <|> Microchip <$ "-compatible microchip"
    label = Text.pack <$> some letterChar

isValid :: State -> Bool
isValid (State _ items) = all isValid' grouped where
    isValid' (gen, chip) = gen == chip || all (\(gen', _) -> chip /= gen') grouped
    grouped = grouped2 $ V.toList items

atSameFloor :: State -> [Int]
atSameFloor (State elevator items) = [ i | (i, f) <- zip [0..] (V.toList items), f==elevator]

sortState :: State -> State
sortState (State e items) = State e items' where
    items' = V.fromList . ungroup2 . sort . grouped2 $ V.toList items 
    ungroup2 [] = []
    ungroup2 ((x,y):xs) = x: y : ungroup2 xs

neighbors :: State -> [State]
neighbors state@(State elevator items) = states where
    states =
        [ sortState state' -- the order of pairs does not matter
        | direction <- [1, - 1]
        , let elevator' = elevator + direction
        , 0 <= elevator' && elevator' <= 3
        , c <- candidates direction (atSameFloor state)
        , let items' = items V.// map (, elevator') c
        , let state' = State elevator' items'
        , isValid state'
        ]
    candidates dir xs =
        if dir == 1
            then map pure xs ++ pairwise (\x y -> [x, y]) xs
            else map pure xs -- do not bring two items downstairs

initialState :: [[Item]] -> State
initialState items = State 0 items'' where
    items'' =
        V.fromList . map snd $ sortOn fst [ (item, i)
                                          | (i, items') <- zip [0..] items
                                          , item <- items'
                                          ]

initialState2 :: [[Item]] -> State
initialState2 items = State 0 (items' <> V.fromList [0, 0, 0, 0])
    where State _ items' = initialState items

isGoal :: State -> Bool
isGoal (State _ items) = all (==3) (V.toList items)

part1 :: [[Item]] -> Maybe Int
part1 items = distance neighbors isGoal (initialState items)
    -- astar (initialState items) isGoal (map (,1) . neighbors) heuristic
    -- bfs is faster than a*

part2 :: [[Item]] -> Maybe Int
part2 items = distance neighbors isGoal (initialState2 items)
  

solve :: Text -> IO ()
solve = aoc parser part1 part2