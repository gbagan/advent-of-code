-- https://adventofcode.com/2020/day/16
module AOC2020.Day16 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Parser (Parser, some, letterChar, char, sepBy1, sepEndBy1, eol, decimal, hspace)
import           AOC.Interval (Interval(..))
import qualified AOC.Interval as I
import           AOC.Search (maximumMatching')
import qualified Data.Vector as V


data Field = Field { _name :: String, _itvs :: [Interval Int] }
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
    itvs = concatMap _itvs fields
    nearby = concat nearbyTickets

part2 :: Input -> Int
part2 (Input fields myTicket nearbyTickets) = 0 where
    itvs = concatMap _itvs fields
    nearbyTickets' = filter (any \t -> all (I.notMember t) itvs) nearbyTickets
    trTickets = zip [0..] . transpose $ myTicket : nearbyTickets'
    itvs' = zip [0..] (map _itvs fields)


solve :: Text -> IO ()
solve = aoc parser part1 part2