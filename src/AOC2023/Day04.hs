-- https://adventofcode.com/2023/day/4
module AOC2023.Day04 (solve) where
import           RIO hiding (some, many)
import           RIO.List (splitAt)
import qualified RIO.HashSet as HS
import           Text.Megaparsec (sepEndBy1, some, many)
import           Text.Megaparsec.Char (char, digitChar, eol, string)
import           Text.Megaparsec.Char.Lexer (decimal)
import           Util (Parser, aoc')

type Card = ([Int], [Int])

parser :: Parser [Card]
parser = card `sepEndBy1` eol where
    card = do
        _ <- string "Card" *> some (char ' ') *> some digitChar *> char ':'
        (,) <$> list <* char '|' <*> list
    list = many (char ' ') *> decimal `sepEndBy1` some (char ' ')

precomp :: [Card] -> [Int]
precomp = map winningNumber where
    winningNumber (owned, winning) = HS.size $ HS.intersection (HS.fromList winning) (HS.fromList owned)

part1 :: [Int] -> Int
part1 = sum . map pow2 where
    pow2 0 = 0
    pow2 n = 2 ^ (n-1)

part2 :: [Int] -> Int
part2 = go . map (,1) where
    go [] = 0
    go ((c, n):xs) = n + go (map (second (+n)) before ++ after)  where
        (before, after) = splitAt c xs

solve :: MonadIO m => Text -> m ()
solve = aoc' parser (Just . precomp) part1 part2