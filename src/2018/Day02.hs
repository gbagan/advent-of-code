-- https://adventofcode.com/2018/day/1
module Day02 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.List (count, freqs, pairwise)
import           AOC.Parser (Parser, sepEndBy1, eol, lowerChar, some)

parser :: Parser [String]
parser = some lowerChar `sepEndBy1` eol

part1 :: [String] -> Int
part1 xs = count (f 2) fs * count (f 3) fs where
    fs = map freqs xs
    f n = any ((== n) . snd)

part2 :: [String] -> Maybe String
part2 = listToMaybe . catMaybes . pairwise go where
    go l1 l2 = if count id (zipWith (/=)  l1 l2) == 1 then Just (common l1 l2) else Nothing
    common l1 l2 = catMaybes $ zipWith (\c1 c2 -> if c1 == c2 then Just c1 else Nothing) l1 l2

solve :: Text -> IO ()
solve = aoc parser part1 part2