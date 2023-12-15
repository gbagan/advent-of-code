-- https://adventofcode.com/2020/day/11
module AOC2020.Day11 (solve) where
import           AOC.Prelude hiding (toList)
import           Data.Massiv.Array (Matrix, (!), (!?), fromLists', toList, makeArray, size, B, Comp(Seq), Ix2(..))
import           AOC (aoc)
import           AOC.Parser (Parser, sepEndBy1, some, eol)
import           AOC.List (count)
import           AOC.V2 (V2(..), surrounding, toIx2)

data Seat = Floor | Empty | Occupied deriving (Eq)

parser :: Parser (Matrix B Seat)
parser = fromLists' Seq <$> some seat `sepEndBy1` eol where
    seat = Floor <$ "." <|> Empty <$ "L"

step1 :: Matrix B Seat -> Matrix B Seat
step1 m = makeArray Seq (size m) \(Ix2 i j) ->
    let nbors = mapMaybe ((m !?) . toIx2) (surrounding (V2 i j)) in
    case m ! Ix2 i j of
        Floor -> Floor
        Empty | Occupied `notElem` nbors -> Occupied
              | otherwise -> Empty
        Occupied | count (==Occupied) nbors >= 4 -> Empty
                 | otherwise -> Occupied

neighborInDirection :: Matrix B Seat -> (V2 Int) -> (V2 Int) -> Maybe Seat
neighborInDirection g p dxy = go (p + dxy) where
    go xy = case g !? toIx2 xy of
        Nothing -> Nothing
        Just Floor -> go (xy+dxy)
        Just s -> Just s

directions :: [V2 Int]
directions = surrounding (V2 0 0)

step2 :: Matrix B Seat -> Matrix B Seat
step2 m = makeArray Seq (size m) \(Ix2 i j) ->
    let nbors = mapMaybe (neighborInDirection m (V2 i j)) directions in
    case m ! Ix2 i j of
        Floor -> Floor
        Empty | Occupied `notElem` nbors -> Occupied
              | otherwise -> Empty
        Occupied | count (==Occupied) nbors >= 5 -> Empty
                 | otherwise -> Occupied

solveFor :: (Matrix B Seat -> Matrix B Seat) -> Matrix B Seat -> Int
solveFor step m = let m' = step m in
    if m == m' then count (==Occupied) (toList m') else solveFor step m'

solve :: Text -> IO ()
solve = aoc parser (solveFor step1) (solveFor step2)