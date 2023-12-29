-- https://adventofcode.com/2016/day/10
module Day10 (solve) where
import           AOC.Prelude
import qualified Data.IntMap.Strict as Map
import           AOC (aoc')
import           AOC.Parser (Parser, decimal, eol, sepEndBy1, format)

data Bin = Bot !Int | Output !Int
data Instr = Goes !Int !Int | Gives !Int !Bin !Bin
type Rules = IntMap (Bin, Bin)

type Input = (Rules, IntMap [Int]) -- rules, bots, outputs

parser :: Parser [Instr]
parser = (goes <|> give) `sepEndBy1` eol where
    goes = [format| $Goes value {decimal} goes to bot {decimal}|]
    give = [format| $giveF bot {decimal} gives low to {bin} {decimal} and high to {bin} {decimal}|]
    bin = Bot <$ "bot" <|> Output <$ "output"
    giveF giver lowBin lowInt highBin highInt = Gives giver (lowBin lowInt) (highBin highInt)


precomp :: [Instr] -> Input 
precomp = foldl' go (Map.empty, Map.empty) where
    go acc = \case
        Goes val bot -> second (Map.insertWith (++) bot [val]) acc
        Gives bot bin1 bin2 -> first (Map.insert bot (bin1,bin2)) acc

step :: Rules -> (IntMap [Int], IntMap [Int]) -> (IntMap [Int], IntMap [Int])
step rules bins = bins

part1 :: Input -> Int
part1 (rules, bot) = 0

part2 :: Input -> Int
part2 _ = 0

solve :: Text -> IO ()
solve = aoc' parser (pure . precomp) part1 part2