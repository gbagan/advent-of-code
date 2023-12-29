-- https://adventofcode.com/2016/day/2
module Day02 (solve) where
import           AOC.Prelude hiding (tail)
import           Data.List (tail)
import           AOC (aoc)
import           AOC.Parser (Parser, eol, sepEndBy1, some)
import           Data.Massiv.Array hiding (tail, toIx2)
import           AOC.V2 (V2(..), north, south, west, east, toIx2)

type Position = V2 Int
type Direction = V2 Int
type Keypad = Matrix B (Maybe Char)

parser :: Parser [[Direction]]
parser = some dir `sepEndBy1` eol where
    dir = west <$ "L" <|> east <$ "R" <|> north <$ "U" <|> south <$ "D"

keypad1 :: Keypad
keypad1 = fromLists' Seq 
    [ [Just '1', Just '2', Just '3']
    , [Just '4', Just '5', Just '6']
    , [Just '7', Just '8', Just '9']
    ]

keypad2 :: Keypad
keypad2 = fromLists' Seq
    [ [Nothing,  Nothing,  Just '1', Nothing,  Nothing]
    , [Nothing,  Just '2', Just '3', Just '4', Nothing]
    , [Just '5', Just '6', Just '7', Just '8', Just '9']
    , [Nothing,  Just 'A', Just 'B', Just 'C', Nothing]
    , [Nothing,  Nothing,  Just 'D', Nothing,  Nothing]
    ]

move :: Keypad -> Position -> Direction -> Position
move keypad pos dir | isNothing (join (keypad !? toIx2 pos')) = pos
                    | otherwise = pos'
    where pos' = pos + dir

solveFor :: Position -> Keypad -> [[Direction]] -> String
solveFor start keypad dirs = concatMap (maybeToList . (keypad !) . toIx2) positions
    where
    positions = tail $ scanl' (foldl' (move keypad)) start dirs

part1, part2 :: [[Direction]] -> String
part1 = solveFor (V2 1 1) keypad1
part2 = solveFor (V2 2 0) keypad2

solve :: Text -> IO ()
solve = aoc parser part1 part2
