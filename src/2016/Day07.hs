-- https://adventofcode.com/2016/day/7
module Day07 (solve) where
import           AOC.Prelude hiding (sequence)
import qualified Data.HashSet as Set
import           AOC (aoc)
import           AOC.Parser (Parser, between, eol, lowerChar, sepEndBy1, some)
import           AOC.List (count)

type IP = ([String], [String]) --  outside square brackets, within square brackets

parser :: Parser [IP]
parser = (partitionEithers <$> some sequence) `sepEndBy1` eol where
    sequence = Left <$> some lowerChar
            <|> Right <$> between "[" "]" (some lowerChar)

containsABBA :: String -> Bool
containsABBA (x:y:z:t:xs) = x == t && y == z && x /= y || containsABBA (y:z:t:xs)
containsABBA _ = False

part1 :: [IP] -> Int
part1 = count supportsTLS where
    supportsTLS (xs, ys) = any containsABBA xs && not (any containsABBA ys)

abaList :: String -> [(Char, Char)]
abaList (x:y:z:xs) | x == z && x /= y = (x, y) : abaList (y:z:xs)
                   | otherwise        = abaList (y:z:xs)
abaList _ = []

part2 :: [IP] -> Int
part2 = count supportsSSL where
    supportsSSL (xs, ys) = not . Set.null $ Set.map swap set1 `Set.intersection` set2
        where
        set1 = Set.unions (map (fromList . abaList) xs)
        set2 = Set.unions (map (fromList . abaList) ys)

solve :: Text -> IO ()
solve = aoc parser part1 part2