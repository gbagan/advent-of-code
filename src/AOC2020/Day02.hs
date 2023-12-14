-- https://adventofcode.com/2020/day/2
module AOC2020.Day02 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Parser (Parser, sepEndBy1, some, eol, lowerChar, decimal)
import           AOC.List (count)

data Input = Input !Int !Int !Char !String

parser :: Parser [Input]
parser = input `sepEndBy1` eol where
    input = do
        x <- decimal
        _ <- "-"
        y <- decimal
        _ <- " "
        c <- lowerChar
        _ <- ": "
        pwd <- some lowerChar
        pure $ Input x y c pwd

check1 :: Input -> Bool
check1 (Input i j c pwd) = i <= k && k <= j where
                              k = count (==c) pwd

check2 :: Input -> Bool
check2 (Input i j c pwd) = (pwd !!? (i-1) == Just c) 
                        /= (pwd !!? (j-1) == Just c)

solve :: Text -> IO ()
solve = aoc parser (count check1) (count check2)
