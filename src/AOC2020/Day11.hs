-- https://adventofcode.com/2020/day/11
module AOC2020.Day11 (solve) where
import           RIO hiding (some)
import           Text.Megaparsec (sepEndBy1, some)
import           Text.Megaparsec.Char (char, eol)
import           Util (Parser, aoc, count, kingAdjacentPoints)
import           Util.Matrix (Matrix)
import qualified Util.Matrix as M

data Seat = Floor | Empty | Occupied deriving (Eq)

parser :: Parser (Matrix Seat)
parser = M.fromList <$> some seat `sepEndBy1` eol where
    seat = Floor <$ char '.' <|> Empty <$ char 'L'

step1 :: Matrix Seat -> Matrix Seat
step1 m = M.generate (M.nbRows m) (M.nbColumns m) \i j ->
    let nbors = mapMaybe (m M.!?) (kingAdjacentPoints (i, j)) in
    case m M.! (i, j) of
        Floor -> Floor
        Empty | Occupied `notElem` nbors -> Occupied
              | otherwise -> Empty
        Occupied | count (==Occupied) nbors >= 4 -> Empty
                 | otherwise -> Occupied

neighborInDirection :: Matrix Seat -> (Int, Int) -> (Int, Int) -> Maybe Seat
neighborInDirection g (ii,jj) (di,dj) = go (ii+di,jj+dj) where
    go (i, j) = case g M.!? (i, j) of
        Nothing -> Nothing
        Just Floor -> go (i+di,j+dj)
        Just s -> Just s

directions :: [(Int, Int)]
directions = [(0, 1), (1, 0), (0, -1), (-1, 0), (1, 1), (1, -1), (-1, 1), (-1, -1)]

step2 :: Matrix Seat -> Matrix Seat
step2 m = M.generate (M.nbRows m) (M.nbColumns m) \i j ->
    let nbors = mapMaybe (neighborInDirection m (i, j)) directions in
    case m M.! (i, j) of
        Floor -> Floor
        Empty | Occupied `notElem` nbors -> Occupied
              | otherwise -> Empty
        Occupied | count (==Occupied) nbors >= 5 -> Empty
                 | otherwise -> Occupied

solve' :: (Matrix Seat -> Matrix Seat) -> Matrix Seat -> Int
solve' step m = let m' = step m in
    if m == m' then count (==Occupied) (M.elems m') else solve' step m'

solve :: MonadIO m => Text -> m ()
solve = aoc parser (solve' step1) (solve' step2)