-- https://adventofcode.com/2016/day/10
module Day10 (solve) where
import           AOC.Prelude hiding (last)
import           Data.List (last)
import qualified Data.IntMap.Strict as Map
import           AOC (aoc')
import           AOC.Parser (Parser, decimal, eol, sepEndBy1, scanf)

data BinType = Bot | Output
data Bin = Bin !BinType !Int
data Instr = Goes !Int !Int | Gives !Int !Bin !Bin
type Rules = IntMap (Bin, Bin)
type Bins = IntMap [Int]

type Input = (Rules, IntMap [Int]) -- rules, bots

parser :: Parser [Instr]
parser = (goes <|> give) `sepEndBy1` eol where
    goes = [scanf| $Goes value {decimal} goes to bot {decimal}|]
    give = [scanf| $giveF bot {decimal} gives low to {bin} {decimal} and high to {bin} {decimal}|]
    bin = Bot <$ "bot" <|> Output <$ "output"
    giveF giver lowBin lowInt highBin highInt = Gives giver (Bin lowBin lowInt) (Bin highBin highInt)


precomp :: [Instr] -> Input
precomp = foldl' go (Map.empty, Map.empty) where
    go acc = \case
        Goes val bot -> second (Map.insertWith (++) bot [val]) acc
        Gives bot bin1 bin2 -> first (Map.insert bot (bin1,bin2)) acc

step :: Rules -> (Bins, Bins) -> Maybe (Bins, Bins)
step rules bins =
    case Map.toList (fst bins) & find \(_, v) -> length v == 2 of
        Nothing -> Nothing
        Just (k, [v1, v2]) -> 
            let vmin = min v1 v2
                vmax = max v1 v2
                (Bin type1 id1, Bin type2 id2) = rules Map.! k
            in Just . first (Map.delete k)
                    . target type1 (Map.insertWith (++) id1 [vmin])
                    . target type2 (Map.insertWith (++) id2 [vmax])
                    $ bins
        _ -> error "step: cannot happen"
    where 
    target Bot = first
    target Output = second

run :: Input -> [(Bins, Bins)]
run (rules, bots) = go (bots, Map.empty) where
    go bins = case step rules bins of
        Nothing -> [bins]
        Just bins' -> bins : go bins'

part1 :: Input -> Maybe Int
part1 input = listToMaybe
    [ k
    | (bots, _) <- run input
    , (k, v) <- Map.toList bots
    , v == [17, 61] || v == [61, 17]
    ]

part2 :: Input -> Int
part2 input = product 
        [ product (outputs Map.! i)
        | i <- [0..2]
        , let (_, outputs) = last (run input)
        ]

solve :: Text -> IO ()
solve = aoc' parser (pure . precomp) part1 part2