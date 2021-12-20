-- https://adventofcode.com/2021/day/1
module Day01 (solve) where
import           Text.Megaparsec (sepEndBy1)
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import           Util (Parser, aocTemplate, count)

parser :: Parser [Int]
parser = L.decimal `sepEndBy1` P.eol

algo :: Int -> [Int] -> Int
algo n l = count id $ zipWith (<) l (drop n l)

solve :: String -> IO ()
solve = aocTemplate parser pure (pure . algo 1) (pure . algo 3)

