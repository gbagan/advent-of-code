-- https://adventofcode.com/2020/day/11
module AOC2020.Day11 (solve) where
import           RIO hiding (some, toList)
import           Text.Megaparsec (sepEndBy1, some)
import           Text.Megaparsec.Char (char, eol)
import           Util (Parser, aoc, count, kingAdjacentPoints)
import           Data.Massiv.Array (Matrix, (!), (!?), fromLists', toList, makeArray, size, B, Comp(Seq), Ix2(..))

data Seat = Floor | Empty | Occupied deriving (Eq)

parser :: Parser (Matrix B Seat)
parser = fromLists' Seq <$> some seat `sepEndBy1` eol where
    seat = Floor <$ char '.' <|> Empty <$ char 'L'

step1 :: Matrix B Seat -> Matrix B Seat
step1 m = makeArray Seq (size m) \(Ix2 i j) ->
    let nbors = mapMaybe ((m !?) . uncurry Ix2) (kingAdjacentPoints (i, j)) in
    case m ! Ix2 i j of
        Floor -> Floor
        Empty | Occupied `notElem` nbors -> Occupied
              | otherwise -> Empty
        Occupied | count (==Occupied) nbors >= 4 -> Empty
                 | otherwise -> Occupied

neighborInDirection :: Matrix B Seat -> (Int, Int) -> (Int, Int) -> Maybe Seat
neighborInDirection g (ii,jj) (di,dj) = go (ii+di,jj+dj) where
    go (i, j) = case g !? Ix2 i j of
        Nothing -> Nothing
        Just Floor -> go (i+di,j+dj)
        Just s -> Just s

directions :: [(Int, Int)]
directions = [(0, 1), (1, 0), (0, -1), (-1, 0), (1, 1), (1, -1), (-1, 1), (-1, -1)]

step2 :: Matrix B Seat -> Matrix B Seat
step2 m = makeArray Seq (size m) \(Ix2 i j) ->
    let nbors = mapMaybe (neighborInDirection m (i, j)) directions in
    case m ! Ix2 i j of
        Floor -> Floor
        Empty | Occupied `notElem` nbors -> Occupied
              | otherwise -> Empty
        Occupied | count (==Occupied) nbors >= 5 -> Empty
                 | otherwise -> Occupied

solveWith :: (Matrix B Seat -> Matrix B Seat) -> Matrix B Seat -> Int
solveWith step m = let m' = step m in
    if m == m' then count (==Occupied) (toList m') else solveWith step m'

solve :: MonadIO m => Text -> m ()
solve = aoc parser (solveWith step1) (solveWith step2)