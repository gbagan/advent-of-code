-- https://adventofcode.com/2023/day/4
module Day04 (solve) where
import           AOC.Prelude
import qualified Data.IntSet as S
import           AOC (aoc')
import           AOC.Parser (Parser, takeWhileP, sepEndBy1, char, eol, hspace, decimal)

type Card = ([Int], [Int]) -- owned numbers, winning numbers

parser :: Parser [Card]
parser = card `sepEndBy1` eol where
    card = do
        _ <- takeWhileP Nothing (/= ':') *> char ':'
        (,) <$> list <* "|" <*> list
    list = hspace *> decimal `sepEndBy1` hspace

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
    go ((score, freq):xs) = freq + go (affected' ++ nonAffected) where
        (affected, nonAffected) = splitAt score xs
        affected' = map (second (+freq)) affected

solve :: Text -> IO ()
solve = aoc' parser (Just . precomp) part1 part2