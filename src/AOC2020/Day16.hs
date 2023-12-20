-- https://adventofcode.com/2020/day/16
module AOC2020.Day16 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Parser (Parser, some, letterChar, char, sepBy1, sepEndBy1, eol, decimal, hspace)
import           AOC.Interval (Interval(..))
import qualified AOC.Interval as I
import           AOC.Search (maximumMatching')
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as Map

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
part2 (Input fields myTicket nearbyTickets) = product values where
    vMyTicket = V.fromList myTicket
    itvs = concatMap _itvs fields
    departures = [i | (i, field) <- zip [0..] fields, "departure" `isPrefixOf` _name field]
    nearbyTickets' = filter (all \t -> any (I.member t) itvs) nearbyTickets
    trTickets = zip [0..] . transpose $ myTicket : nearbyTickets'
    itvs' = map _itvs fields
    graph = itvs' <&> \itvs'' -> trTickets & mapMaybe \(j, tickets) ->
        if all (\ticket -> any (I.member ticket) itvs'') tickets
            then Just j
            else Nothing
    graph' = (V.length vMyTicket, V.fromList graph)
    m = maximumMatching' graph'
    values = departures <&> \i -> vMyTicket V.! (m Map.! i)

solve :: Text -> IO ()
solve = aoc parser part1 part2