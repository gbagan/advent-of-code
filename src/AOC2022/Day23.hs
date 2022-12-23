-- https://adventofcode.com/2022/day/23
module AOC2022.Day23 (solve) where
import           RIO hiding (some)
import           RIO.List (cycle, scanl')
import           RIO.List.Partial (minimum, maximum, (!!))
import qualified RIO.HashMap.Partial as Map ((!))
import qualified RIO.HashSet as Set
import           Text.Megaparsec (sepEndBy1, some)
import           Text.Megaparsec.Char (char, eol)
import           Linear.V2 (V2(..))
import           Data.List.Split (divvy)
import           Util (Parser, aoc, freqs, kingAdjacentPoints')

type Rule = HashSet (V2 Int) -> V2 Int -> Maybe (V2 Int)

parser :: Parser (HashSet (V2 Int))
parser = listToV2Set <$> some tile `sepEndBy1` eol where
    tile = False <$ char '.' <|> True <$ char '#'


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
    [   \elves elf -> if not $ any (`Set.member` elves) [elf - V2 1 i | i <- [-1..1]]
                     then Just (elf - V2 1 0)
                     else Nothing
    ,   \elves elf -> if not $ any (`Set.member` elves) [elf + V2 1 i | i <- [-1..1]]
                     then Just (elf + V2 1 0)
                     else Nothing
    ,   \elves elf -> if not $ any (`Set.member` elves) [elf - V2 i 1 | i <- [-1..1]]
                     then Just (elf - V2 0 1)
                     else Nothing
    ,   \elves elf -> if not $ any (`Set.member` elves) [elf + V2 i 1 | i <- [-1..1]]
                     then Just (elf + V2 0 1)
                     else Nothing
    ]

runRound :: [Rule] -> HashSet (V2 Int) -> HashSet (V2 Int)
runRound rules' elves = elves' where
    transitions = elves & Set.map \elf ->
        if not $ any (`Set.member` elves) (kingAdjacentPoints' elf)
        then (elf, elf)
        else (elf, fromMaybe elf . listToMaybe $ catMaybes [rule elves elf | rule <- rules'])
    elfMap = freqs . map snd $ Set.toList transitions
    elves' = transitions & Set.map \(elf, elf') ->
        if elfMap Map.! elf' >= 2 then elf else elf'

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

solve :: MonadIO m => Text -> m ()
solve = aoc parser part1 part2
