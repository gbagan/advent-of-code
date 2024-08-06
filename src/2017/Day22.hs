-- https://adventofcode.com/2017/day/22
module Day22 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Parser (Parser, eol, sepEndBy1, some)
import qualified Data.HashMap.Strict as Map
import           AOC.Util (listTo2dMap)
import           AOC.V2 (V2, north, turnLeft, turnRight)
import           Data.List (maximum)

data Node = Clean | Infected | Weakened | Flagged deriving (Eq)

parser :: Parser (HashMap (V2 Int) Node)
parser = listTo2dMap <$> some node `sepEndBy1` eol where
    node = Infected <$ "#" <|> Clean <$ "."

turn :: Node -> V2 Int -> V2 Int
turn Clean    = turnLeft
turn Infected = turnRight
turn Weakened = id
turn Flagged  = negate

rule1 :: Node -> Node
rule1 Clean = Infected
rule1 _     = Clean

rule2 :: Node -> Node
rule2 Clean = Weakened
rule2 Weakened = Infected
rule2 Infected = Flagged
rule2 Flagged = Clean

solveFor :: Int -> (Node -> Node) -> HashMap (V2 Int) Node -> Int
solveFor nbSteps rule initialNodes = go initialNodes nbSteps 0 mid north where
    mid = fmap (`div` 2) . maximum $ Map.keys initialNodes
    go !nodes !remainingSteps !nbInfection !pos !dir
        | remainingSteps == 0 = nbInfection
        | otherwise = go nodes' (remainingSteps-1) nbInfection' pos' dir'
        where
        node = Map.findWithDefault Clean pos nodes
        node' = rule node
        nodes' = Map.insert pos node' nodes
        nbInfection' = if node' == Infected then nbInfection + 1 else nbInfection
        dir' = turn node dir
        pos' = pos + dir'

solve :: Text -> IO ()
solve = aoc parser (solveFor 10_000 rule1) (solveFor 10_000_000 rule2)