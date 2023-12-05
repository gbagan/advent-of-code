-- https://adventofcode.com/2023/day/4
module AOC2023.Day04 (solve) where
import           RIO hiding (some, many)
import           RIO.List (splitAt)
import qualified Data.IntSet as S
import           Text.Megaparsec (anySingleBut, sepEndBy1, some, many)
import           Text.Megaparsec.Char (char, eol)
import           Text.Megaparsec.Char.Lexer (decimal)
import           Util (Parser, aoc')

type Card = ([Int], [Int])

parser :: Parser [Card]
parser = card `sepEndBy1` eol where
    card = do
        _ <- some (anySingleBut ':') *> char ':'
        (,) <$> list <* char '|' <*> list
    list = many (char ' ') *> decimal `sepEndBy1` some (char ' ')

precomp :: [Card] -> [Int]
precomp = map score where
    score (owned, winning) = S.size $ S.intersection (S.fromList winning) (S.fromList owned)

part1 :: [Int] -> Int
part1 = sum . map pow2 where
    pow2 0 = 0
    pow2 n = 2 ^ (n-1)

part2 :: [Int] -> Int
part2 = go . map (,1) where
    go [] = 0
    go ((score, freq):xs) = freq + go (before' ++ after) where
        (before, after) = splitAt score xs
        before' = map (second (+freq)) before

solve :: MonadIO m => Text -> m ()
solve = aoc' parser (Just . precomp) part1 part2