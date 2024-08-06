-- https://adventofcode.com/2019/day/13
module Day13 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Parser (Parser, signedDecimal, sepBy1)
import           AOC.IntCode (Effect(..), newMachine, runProgram, runEffect, set)
import           AOC.List (count, grouped3)
import           AOC.V2 (V2(..))
import qualified Data.HashMap.Strict as Map

parser :: Parser [Int]
parser = signedDecimal `sepBy1` ","

getScreen :: [Int] -> HashMap (V2 Int) Int
getScreen = foldl' go Map.empty . grouped3 . runProgram [] where
    go screen (x, y, t) = Map.insert (V2 y x) t screen

part1 :: [Int] -> Int
part1 = count (==2) . Map.elems . getScreen

part2 :: [Int] -> Int
part2 pgm = go (runEffect machine) 0 0 0 where
    machine = set 0 2 $ newMachine pgm
    go effect ball paddle score = case effect of
        Halt _ -> score
        Input f -> go (f tilt) ball paddle score where
            tilt = signum (ball - paddle)
        Output (-1) (Output 0 (Output score' effect')) -> go effect' ball paddle score'
        Output x (Output _ (Output 3 effect'))         -> go effect' ball x score
        Output x (Output _ (Output 4 effect'))         -> go effect' x paddle score
        Output _ (Output _ (Output _ effect'))         -> go effect' ball paddle score
        _ -> error "part 2: illegal program"


solve :: Text -> IO ()
solve = aoc parser part1 part2