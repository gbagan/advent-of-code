-- https://adventofcode.com/2018/day/4
module Day04 (solve) where
import           AOC.Prelude hiding (guard, head)
import           AOC (aoc')
import           AOC.List (mostCommon)
import           AOC.Parser (Parser, sepEndBy1, eol, decimal, scanf)

type Time = (Int, Int, Int, Int, Int)
data Event = Shift Int | Asleep | WakeUp

parser :: Parser [(Time, Event)]
parser = [scanf|{time} {event}|] `sepEndBy1` eol where
    time = [scanf|[{decimal}-{decimal}-{decimal} {decimal}:{decimal}]|]
    event = Shift <$> [scanf|Guard #{decimal} begins shift|]
            <|> "falls asleep" $> Asleep
            <|> "wakes up" $> WakeUp

getAsleepGuardsAndMinutes :: [(Time, Event)] -> [(Int, Int)]
getAsleepGuardsAndMinutes = go Nothing Nothing . sortOn fst where
    go _ _ [] = []
    go _ _ ((_, Shift guard) : xs) = go (Just guard) Nothing xs
    go guard _ (((_,_, _, _, s), Asleep) : xs) = go guard (Just s) xs
    go (Just guard) (Just asleep) (((_,_, _, _, s), WakeUp) : xs) = map (guard,) [asleep..s-1] ++ go (Just guard) Nothing xs
    go _ _ _ = error "getAsleepDurations"


part1 :: [(Int, Int)] -> Maybe Int
part1 guardsAndMinutes = do
    guard <- mostCommon (map fst guardsAndMinutes)
    minute <- mostCommon [m | (g, m) <- guardsAndMinutes, g == guard]
    Just $ guard * minute

part2 :: [(Int, Int)] -> Maybe Int
part2 guardsAndMinutes = do 
    (guard, minute) <- mostCommon guardsAndMinutes
    Just $ guard * minute

solve :: Text -> IO ()
solve = aoc' parser (Just . getAsleepGuardsAndMinutes) part1 part2