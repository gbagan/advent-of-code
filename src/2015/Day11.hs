-- https://adventofcode.com/2015/day/11
module Day11 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.List (drop1)
import           AOC.Parser (Parser, alphaNumChar, some)

parser :: Parser String
parser = some alphaNumChar

has2Pairs, hasStraight, hasNoIOL, isValid :: String -> Bool

has2Pairs = aux Nothing where
    aux (Just z) (x:y:_) | x==y && x /= z = True
    aux mb (x:y:xs) | x==y = aux (Just x) xs
                    | otherwise = aux mb (y:xs)
    aux _ _ = False

hasStraight (x:y:z:xs) = y == pred x && z == pred y || hasStraight (y:z:xs)
hasStraight _ = False

hasNoIOL = all (\c -> c /= 'i' && c /= 'o' && c /= 'l')

isValid s = has2Pairs s && hasStraight s && hasNoIOL s

nextString :: String -> String
nextString "z" = "aa"
nextString ('z' : xs) = 'a' : nextString xs
nextString (x : xs) = succ x : xs
nextString "" = error "nextString on an empty string" 

nextPassword :: String -> Maybe String
nextPassword = fmap reverse . find isValid . drop1 . iterate' nextString  . reverse 

part1, part2 :: String -> Maybe String
part1 = nextPassword
part2 = nextPassword  <=< nextPassword

solve :: Text -> IO ()
solve = aoc parser part1 part2