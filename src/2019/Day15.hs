-- https://adventofcode.com/2019/day/15
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Day15 (solve) where
import           AOC.Prelude
import           AOC (aoc_)
import           AOC.Parser (Parser, signedDecimal, sepBy1)
import           AOC.List (lastMaybe)
import           AOC.IntCode (Effect(..), newMachine, runEffect)
import           AOC.V2 (V2, origin, west, east, north, south)
import           AOC.Graph (bfsOn)

parser :: Parser [Int]
parser = signedDecimal `sepBy1` ","

data SearchState = SearchState
    { onOxygen :: !Bool
    , position :: !(V2 Int)
    , effect   :: Effect
    }

neighbors :: SearchState -> [SearchState]
neighbors (SearchState _ loc eff) = case eff of
    Input f -> do
        (i, dir) <- [(1,north), (2,south), (3,west), (4,east)]
        case f i of
            Output output eff' -> do
                guard $ output /= 0
                pure $ SearchState (output == 2) (loc + dir) eff'
            _ -> error "incorrect program"
    _ -> error "incorrect program"

solve' :: [Int] -> Maybe (Int, Int)
solve' pgm = do
    let eff = runEffect $ newMachine pgm
    let states = bfsOn position neighbors (SearchState False origin eff)
    (p1, st) <- find (onOxygen . snd) states
    p2 <- fst <$> lastMaybe (bfsOn position neighbors st)
    pure (p1, p2)

solve :: Text -> IO ()
solve = aoc_ parser solve'