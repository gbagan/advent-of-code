-- https://adventofcode.com/2020/day/16
module Day16 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Parser (Parser, some, letterChar, char, sepBy1, sepEndBy1, eol, decimal, hspace)
import           AOC.Range (Range(..))
import qualified AOC.Range as R
import           AOC.Graph (perfectMatchings)

data Field = Field { _name :: String, _ranges :: [Range Int] }
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
    interval = Range <$> decimal <* "-" <*> decimal

part1 :: Input -> Int
part1 (Input fields _ nearbyTickets) = sum [n | n <- nearby, all (R.notMember n) itvs]
    where
    itvs = concatMap _ranges fields
    nearby = concat nearbyTickets

match :: Int -> Field -> Bool
match x (Field _ ranges) = any (R.member x) ranges

matchedFields :: [Field] -> [Int] -> [String]
matchedFields fields col = [_name field | field <- fields, all (`match` field) col]

part2 :: Input -> Maybe Int
part2 (Input fields myTicket nearbyTickets) = do
    let goodTickets = filter (all (\x -> any (match x) fields)) nearbyTickets
    let graph = zip myTicket [matchedFields fields col | col <- transpose goodTickets]
    matching <- listToMaybe $ perfectMatchings graph 
    Just $! product [val | (val, name) <- matching, "departure" `isPrefixOf` name]

solve :: Text -> IO ()
solve = aoc parser part1 part2