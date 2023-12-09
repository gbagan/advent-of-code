-- https://adventofcode.com/2022/day/23
module AOC2022.Day23 (solve) where
import           AOC.Prelude
import           Data.List (minimum, maximum, (!!))
import qualified Data.HashMap.Strict as Map ((!))
import qualified Data.HashSet as Set
import           Linear.V2 (V2(..))
import           Data.List.Split (divvy)
import           AOC (aoc)
import           AOC.Util (freqs, kingAdjacentPoints')
import           AOC.Parser (Parser, sepEndBy1, eol, some)

type Rule = ([V2 Int], V2 Int)

parser :: Parser (HashSet (V2 Int))
parser = listToV2Set <$> some tile `sepEndBy1` eol where
    tile = False <$ "." <|> True <$ "#"

listToV2Set :: [[Bool]] -> HashSet (V2 Int)
listToV2Set l =
    Set.fromList
        [ V2 i j
        | (i, row) <- zip [0..] l
        , (j, v) <- zip [0..] row
        , v
        ]

rules :: [Rule]
rules = cycle
    [ ([V2 (-1) i | i <- [-1..1]], V2 (-1) 0)
    , ([V2 1 i | i <- [-1..1]], V2 1 0)
    , ([V2 i (-1) | i <- [-1..1]], V2 0 (-1))
    , ([V2 i 1 | i <- [-1..1]], V2 0 1)
    ]

runRound :: [Rule] -> HashSet (V2 Int) -> HashSet (V2 Int)
runRound rules' elves = elves' where
    transitions = elves & Set.map \elf ->
        if not $ any (`Set.member` elves) (kingAdjacentPoints' elf)
        then (elf, elf)
        else (elf, fromMaybe elf . listToMaybe $ mapMaybe (applyRule elf) rules')
    applyRule elf (checks, move) = if not $ any ((`Set.member` elves) . (+elf)) checks
                     then Just (elf + move)
                     else Nothing
    elfCounter = freqs . map snd $ Set.toList transitions
    elves' = transitions & Set.map \(elf, elf') ->
        if elfCounter Map.! elf' >= 2 then elf else elf'

simulate :: HashSet (V2 Int) -> [HashSet (V2 Int)]
simulate elves = scanl' (flip runRound) elves rules' where
    rules' = divvy 4 1 rules

part1 :: HashSet (V2 Int) -> Int
part1 elves = (maxr - minr + 1) * (maxc - minc + 1) - Set.size elves' where
    elves' = simulate elves !! 10
    minr = minimum [r | V2 r _ <- Set.toList elves']
    maxr = maximum [r | V2 r _ <- Set.toList elves']
    minc = minimum [c | V2 _ c <- Set.toList elves']
    maxc = maximum [c | V2 _ c <- Set.toList elves']

findFixPointIndex :: Eq a => [a] -> Int
findFixPointIndex xs = go (zip [0..] xs) where
    go ((_, x):(i, y):ys) | x == y = i
                          | otherwise = go $ (i, y):ys
    go _ = undefined -- cannot happen

part2 :: HashSet (V2 Int) -> Int
part2 =  findFixPointIndex . simulate

solve :: Text -> IO ()
solve = aoc parser part1 part2
