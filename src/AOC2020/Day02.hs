-- https://adventofcode.com/2020/day/2
module AOC2020.Day02 (solve) where
import           RIO hiding (some)
import           Text.Megaparsec (sepEndBy1, some)
import           Text.Megaparsec.Char (char, eol, lowerChar, string)
import           Text.Megaparsec.Char.Lexer (decimal)
import           Util (Parser, aoc, count)
import           RIO.Lens (ix) 

data Input = Input !Int !Int !Char !String

parser :: Parser [Input]
parser = input `sepEndBy1` eol where
    input = do
        x <- decimal
        _ <- char '-'
        y <- decimal
        _ <- char ' '
        c <- lowerChar
        _ <- string ": "
        pwd <- some lowerChar
        pure $ Input x y c pwd

check1 :: Input -> Bool
check1 (Input i j c pwd) = i <= k && k <= j where
                              k = count (==c) pwd

check2 :: Input -> Bool
check2 (Input i j c pwd) = (pwd ^? ix (i-1) == Just c) 
                        /= (pwd ^? ix (j-1) == Just c)

solve :: MonadIO m => Text -> m ()
solve = aoc parser (count check1) (count check2)
