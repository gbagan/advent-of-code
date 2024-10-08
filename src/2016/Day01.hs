-- https://adventofcode.com/2016/day/1
module Day01 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Parser (Parser, decimal, sepBy1)
import           AOC.List (findDuplicate)
import           AOC.V2 (V2, manhattan, origin, north, turnLeft, turnRight)

data Turn = L | R
data Step = Step !Turn !Int
type Direction = V2 Int

parser :: Parser [Step]
parser = step `sepBy1` ", " where
    step = Step <$> turn <*> decimal
    turn = L <$ "L" <|> R <$ "R"

nextDir :: Turn -> Direction -> Direction
nextDir L = turnLeft
nextDir R = turnRight 

part1 :: [Step] -> Int
part1 = manhattan origin . fst . foldl' go (origin, north) where
    go (pos, dir) (Step turn n) = (pos + fmap (*n) dir', dir') where
        dir' = nextDir turn dir

stepsToDirs :: Direction -> [Step] -> [Direction]
stepsToDirs _ [] = []
stepsToDirs dir (Step turn n:xs) = replicate n dir' ++ stepsToDirs dir' xs
    where dir' = nextDir turn dir

part2 :: [Step] -> Maybe Int
part2 = fmap (manhattan origin) . findDuplicate . scanl' (+) origin . stepsToDirs north

solve :: Text -> IO ()
solve = aoc parser part1 part2
