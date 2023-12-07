-- https://adventofcode.com/2023/day/8
module AOC2023.Day08 (solve) where
import           RIO hiding (some)
import           RIO.Partial (fromJust)
import           RIO.List (cycle, findIndex, scanl')
import           RIO.List.Partial (last)
import qualified RIO.HashMap as Map
import           RIO.HashMap.Partial ((!))
import           Text.Megaparsec (sepEndBy1, some)
import           Text.Megaparsec.Char (eol, upperChar)
import           Util (Parser, aoc)

data Instr = L | R
data Node = Node String String String
data Input = Input [Instr] [Node]

parser :: Parser Input
parser = Input <$> some instr <* eol <* eol <*> node `sepEndBy1` eol where
    instr = L <$ "L" <|> R <$ "R"
    node = Node <$> address <* " = (" <*> address <* ", " <*> address <* ")"
    address = some upperChar

solveWith :: (String -> Bool) -> (String -> Bool) -> Input -> Int
solveWith startPred endPred (Input instrs nodes) = foldl' lcm 1 steps where
    starts = filter startPred $ Map.keys nodeDict
    steps = [ fromJust $ findIndex endPred walk
            | start <- starts
            , let walk = scanl' go start (cycle instrs)
            ]
    go address = goNext (nodeDict ! address)
    goNext (Node _ left right) = \case
        L -> left
        R -> right
    nodeDict = Map.fromList [(source, node) | node@(Node source _ _) <- nodes]

part1 :: Input -> Int
part1 = solveWith (=="AAA") (=="ZZZ")

-- only works because the length to go from 'A' to 'Z' is the same as the lenght of a loop
part2 :: Input -> Int
part2 = solveWith ((=='A') . last) ((=='Z') . last)

solve :: MonadIO m => Text -> m ()
solve = aoc parser part1 part2
