-- https://adventofcode.com/2017/day/25
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Day25 (solve) where
import           AOC.Prelude hiding (state)
import           AOC (aoc)
import           AOC.Parser (Parser, sepEndBy1, eol, decimal, hspace, upperChar, scanf)
import qualified Data.HashMap.Strict as Map
import           AOC.List (count)
import           AOC.Util (times)

data Direction = DLeft | DRight deriving (Eq) 
type Transition = (Int, Int, Direction, Char)
data Machine = Machine { init :: Char, transitions :: HashMap Char (Transition, Transition) }
data Tape = Tape [Int] Int [Int] Char

parser :: Parser (Machine, Int)
parser = do
    initState <- [scanf|Begin in state {upperChar}.|] <* eol
    nbSteps <- [scanf|Perform a diagnostic checksum after {decimal} steps.|] <* eol <* eol
    st <- state_ `sepEndBy1` (eol <* eol)
    pure (Machine initState (Map.fromList st), nbSteps)
    where
    state_ = (,) <$> [scanf|In state {upperChar}:|] <* eol <*> ((,) <$> transition <* eol <*> transition)
    transition = do
        current <- hspace *> [scanf|If the current value is {decimal}:|] <* eol
        write <- hspace *> [scanf|- Write the value {decimal}.|] <* eol
        move <- hspace *> [scanf|- Move one slot to the {direction}.|] <* eol
        continue <- hspace *> [scanf|- Continue with state {upperChar}.|]
        pure (current, write, move, continue)
    direction = DLeft <$ "left" <|> DRight <$ "right" 

step :: Machine -> Tape -> Tape
step (Machine _ transitions) (Tape left current right state) = Tape left' current' right' state'
    where
    (tr0, tr1) = transitions Map.! state
    (_, toWrite, move, state') = if current == 0 then tr0 else tr1
    (left', current', right') =
        if move == DLeft
            then case left of
                [] -> ([], 0, toWrite : right)
                (x:xs) -> (xs, x, toWrite : right)
            else case right of
                [] -> (toWrite : left, 0, [])
                (x:xs) -> (toWrite : left, x, xs)

checksum :: Tape -> Int
checksum (Tape left current right _) = count (==1) left + current + count (==1) right

part1 :: (Machine, Int) -> Int
part1 (machine, nbSteps) = checksum (times nbSteps (step machine) initTape) where
    initTape = Tape [] 0 [] machine.init

solve :: Text -> IO ()
solve = aoc parser part1 (const (0::Int))