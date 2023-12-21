-- https://adventofcode.com/2020/day/16
module Day16 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Parser (Parser, some, letterChar, char, sepBy1, sepEndBy1, eol, decimal, hspace)
import           AOC.Interval (Interval(..))
import qualified AOC.Interval as I
import           AOC.Search (maximumMatching)
import qualified Data.HashMap.Strict as Map

data Field = Field { _name :: String, _ranges :: [Interval Int] }
data Input = Input [Field] [Int] [[Int]]

parser :: Parser Input
parser = do
    fs <- field `sepEndBy1` eol <* eol 
    ticket <- "your ticket:" *> eol *> intlist
    nearby <- eol *> eol *> "nearby tickets:" *> eol *> intlist `sepEndBy1` eol 
    pure $ Input fs ticket nearby
    where
    intlist = decimal `sepBy1` ","
    field = Field <$> some (letterChar <|> char ' ') <* ":" <* hspace <*> interval `sepBy1` " or "
    interval = Interval <$> decimal <* "-" <*> decimal

part1 :: Input -> Int
part1 (Input fields _ nearbyTickets) = sum [n | n <- nearby, all (I.notMember n) itvs]
    where
    itvs = concatMap _ranges fields
    nearby = concat nearbyTickets

match :: Int -> Field -> Bool
match x (Field _ ranges) = any (I.member x) ranges

matchedFields :: [Field] -> [Int] -> [String]
matchedFields fields col = [_name field | field <- fields, all (`match` field) col]

part2 :: Input -> Int
part2 (Input fields myTicket nearbyTickets) = product values where
    goodTickets = filter (all (\x -> any (match x) fields)) nearbyTickets
    graph = Map.fromList $ zip myTicket [matchedFields fields col | col <- transpose goodTickets]
    matching = maximumMatching graph -- assume this is a perfect matching
    values = [val | (val, name) <- Map.toList matching, "departure" `isPrefixOf` name]  

solve :: Text -> IO ()
solve = aoc parser part1 part2