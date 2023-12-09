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

solveWith :: (Address -> Bool) -> (Address -> Bool) -> Input -> Int
solveWith startPred endPred (Input instrs network) = foldl' lcm 1 periods where
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
part1 = solveWith (=="AAA") (=="ZZZ")

-- only works because the input is nice
part2 :: Input -> Int
part2 = solveWith ((=='A') . last) ((=='Z') . last)

solve :: MonadIO m => Text -> m ()
solve = aoc parser part1 part2