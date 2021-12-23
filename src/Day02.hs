-- https://adventofcode.com/2021/day/2
module Day02 (solve) where
import           RIO
import           Text.Megaparsec (sepEndBy1)
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import           Util (Parser, aocTemplate)

data Instr = Forward Int | IDown Int

parser :: Parser [Instr]
parser = line `sepEndBy1` P.eol where
    line = Forward <$> (P.string "forward " *> L.decimal)
       <|> IDown <$> (P.string "down " *> L.decimal)
       <|> IDown . negate <$> (P.string "up " *> L.decimal)

part1 :: [Instr] -> Int
part1 l = tx * ty where
        (tx, ty) =
            foldl' (\(x, y) -> \case
                Forward v -> (x + v, y)
                IDown v   -> (x, y + v)
            ) (0, 0) l

part2 :: [Instr] -> Int
part2 l = tx * ty where
        (tx, ty, _) =
            foldl' (\(x, y, aim) -> \case
                Forward v -> (x + v, y + aim * v, aim)
                IDown v   -> (x, y, aim + v)
            ) (0, 0, 0) l

solve :: (HasLogFunc env) => Text -> RIO env ()
solve = aocTemplate parser pure (pure . part1) (pure . part2)
