-- https://adventofcode.com/2023/day/8
module Day08 (solve) where
import           AOC.Prelude hiding (last)
import           Data.List (last)
import           Data.Maybe (fromJust)
import qualified Data.HashMap.Strict as Map
import           Data.HashMap.Strict ((!))
import           AOC (aoc)
import           AOC.Parser (Parser, sepEndBy1, some, eol, upperChar)

type Address = String
data Instr = L | R
type Network = HashMap Address (Address, Address)
data Input = Input [Instr] Network

parser :: Parser Input
parser = Input <$> some instr <* eol <* eol <*> network where
    network = networkFromList <$> node `sepEndBy1` eol
    instr = L <$ "L" <|> R <$ "R"
    node = (,,) <$> address <* " = (" <*> address <* ", " <*> address <* ")"
    address = some upperChar
    networkFromList nodes = Map.fromList [(source, (left, right)) | (source, left, right) <- nodes]

solveFor :: (Address -> Bool) -> (Address -> Bool) -> Input -> Int
solveFor startPred endPred (Input instrs network) = foldl' lcm 1 periods where
    starts = filter startPred (Map.keys network)
    periods = [ fromJust $ findIndex endPred walk
              | start <- starts
              , let walk = scanl' move start (cycle instrs)
              ]
    move address = \case
        L -> left
        R -> right
        where (left, right) = network ! address

part1 :: Input -> Int
part1 = solveFor (=="AAA") (=="ZZZ")

-- only works because the input is nice
part2 :: Input -> Int
part2 = solveFor ((=='A') . last) ((=='Z') . last)

solve :: Text -> IO ()
solve = aoc parser part1 part2
