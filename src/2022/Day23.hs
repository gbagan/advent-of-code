-- https://adventofcode.com/2022/day/23
module Day23 (solve) where
import           AOC.Prelude
import           Data.List (minimum, maximum, (!!))
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import           AOC (aoc)
import           AOC.List (freqs, headMaybe, sliding)
import           AOC.Parser (Parser, sepEndBy1, eol, some)
import           AOC.V2 (V2(..), surrounding)

type Rule = ([V2 Int], V2 Int)

parser :: Parser (HashSet (V2 Int))
parser = listToV2Set <$> some tile `sepEndBy1` eol where
    tile = False <$ "." <|> True <$ "#"

listToV2Set :: [[Bool]] -> HashSet (V2 Int)
listToV2Set l =
    Set.fromList
        [ V2 i j
        | (i, row) <- zip [0..] l
        , (j, True) <- zip [0..] row
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
        if not $ any (`Set.member` elves) (surrounding elf)
        then (elf, elf)
        else (elf, fromMaybe elf . headMaybe $ mapMaybe (applyRule elf) rules')
    applyRule elf (checks, move) = if not $ any ((`Set.member` elves) . (+elf)) checks
                     then Just (elf + move)
                     else Nothing
    elfCounter = Map.fromList . freqs . map snd $ Set.toList transitions
    elves' = transitions & Set.map \(elf, elf') ->
        if elfCounter Map.! elf' >= 2 then elf else elf'

simulate :: HashSet (V2 Int) -> [HashSet (V2 Int)]
simulate elves = scanl' (flip runRound) elves rules' where
    rules' = sliding 4 rules

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
    go _ = error "findFixPointIndex: not an infinite list"

part2 :: HashSet (V2 Int) -> Int
part2 =  findFixPointIndex . simulate

solve :: Text -> IO ()
solve = aoc parser part1 part2
