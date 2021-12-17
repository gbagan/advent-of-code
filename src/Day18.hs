module Day18 (solve) where
import           Text.Megaparsec (sepEndBy1, some, (<|>))
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import           Util (Parser, aocTemplate)

type Input = ()

parser :: Parser Input
parser = return ()

part1 :: Input -> Maybe Int
part1 _ = Nothing

part2 :: Input -> Maybe Int
part2 _ = Nothing

solve :: String -> IO ()
solve = aocTemplate parser part1 part2