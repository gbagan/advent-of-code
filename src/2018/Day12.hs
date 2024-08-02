-- https://adventofcode.com/2018/day/12
module Day12 (solve) where
import           AOC.Prelude hiding (show, State)
import           AOC (aoc')
import           AOC.Parser (Parser, eol, sepEndBy1, some, scanf)
import           AOC.Util (binToInt)
import qualified Data.Vector.Unboxed as V
import           Data.List (dropWhileEnd, (!!))

type RawInput = ([Bool], [([Bool], Bool)])
type Input = ([Bool], V.Vector Bool)

data State = State !Int ![Bool]

parser :: Parser RawInput
parser = (,) <$> initial <*> rules where
    initial = [scanf|initial state: {plants}|] <* eol <* eol
    rules = [scanf|{plants} => {plant}|] `sepEndBy1` eol
    plant = True <$ "#" <|> False <$ "."
    plants = some plant

rawToInput :: RawInput -> Input
rawToInput (initial, rules) = (initial, rules') where
    rules' = V.generate 32 (`elem` plants)
    plants = [binToInt ps | (ps, True) <- rules]

score :: State -> Int
score (State n xs) = sum [i | (i, True) <- zip [n..] xs]


trimState :: State -> State
trimState (State n xs) = State (n + length ys) (dropWhileEnd not zs) where
    (ys,zs) = break id xs


step :: V.Vector Bool -> State -> State
step rules (State n xs) = trimState (State (n-3) ys) where
    ys = map (rules V.!) . scanl' nextInt 0 $ xs ++ [False, False, False, False]
    nextInt :: Int -> Bool -> Int
    nextInt x b = (2 * x + fromEnum b) `rem` 32

part1 :: Input -> Int
part1 (initial, rules) = score $ iterate' (step rules) (State 0 initial) !! 20

nbSteps :: Int
nbSteps = 50_000_000_000

part2 :: Input -> Maybe Int
part2 (initial, rules) = do
    let states = iterate' (step rules) (State 0 initial)
    let ys = zip3 [0..] states (drop 1 states)
    (idx, st1, st2) <- find (\(_, State _ bs, State _ bs') -> bs == bs') ys
    let score1 = score st1
    let score2 = score st2
    Just $ score1 + (nbSteps - idx) * (score2 - score1)

solve :: Text -> IO ()
solve = aoc' parser (Just . rawToInput) part1 part2

