module Day21 (solve) where
import           AOC.Prelude
-- import           Data.Array (Array, listArray, range, (!))
import           Data.Massiv.Array (Array, (!), makeArray, BL, Comp(Seq), Ix4, IxN(Ix4), Sz(Sz4))
import           AOC.V2 (V2(..))
import           AOC (aoc)
import           AOC.Parser (Parser, decimal, eol)
import           AOC.List (freqs)

parser :: Parser (Int, Int)
parser = (,) <$ "Player 1 starting position: " <*> decimal <* eol
             <* "Player 2 starting position: " <*> decimal

part1 :: (Int, Int) -> Int
part1 (p1, p2) = go 0 (p1-1) (p2-1) 0 0 where
    go nbRolls pos1 pos2 score1 score2 
        | score1 >= 1000 = score2 * nbRolls
        | score2 >= 1000 = score1 * nbRolls
        | otherwise      = let pos1' = (pos1 + 3 * nbRolls + 6) `mod` 10 in
                            go (nbRolls+3) pos2 pos1' score2 (score1 + pos1' + 1)

diceFreq :: [(Int, Int)]
diceFreq = freqs [x+y+z | x <- [1..3], y <- [1..3], z <- [1..3]]

scores :: Array BL Ix4 (V2 Int)
scores = makeArray Seq (Sz4 10 10 31 31) \(Ix4 pos1 pos2 score1 score2) ->
    if | score1 >= 21 -> V2 1 0
       | score2 >= 21 -> V2 0 1
       | otherwise ->
            sum
            [   V2 (c1 * freq) (c2 * freq)
            |   (dicesum, freq) <- diceFreq
            ,   let pos1' = (pos1 + dicesum) `mod` 10
                    V2 c2 c1 = scores ! Ix4 pos2 pos1' score2 (score1 + pos1' + 1)
            ]
--    | (pos1, pos2, score1, score2) <- range bds
--    ] where bds = ((0, 0, 0, 0), (9, 9, 30, 30))

part2 :: (Int, Int) -> Int
part2 (p1, p2) = max x y where V2 x y = scores ! Ix4 (p1-1) (p2-1) 0 0

solve :: Text -> IO ()
solve = aoc parser part1 part2