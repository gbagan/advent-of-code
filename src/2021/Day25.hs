module Day25  where
import           AOC.Prelude
import           Data.List (maximum)
import qualified Data.HashMap.Strict as Map
import           AOC (aoc)
import           AOC.Parser (Parser, choice, sepEndBy1, eol, some)
import           AOC.V2 (V2(..))
import           AOC.Util (listTo2dMap)

type Coord = V2 Int
data Cell = Empty | East | South deriving (Eq, Ord)
type Board = HashMap Coord Cell
data Input = Input !Int !Int !Board

parser :: Parser Input
parser = withDimensions . listTo2dMap <$> line `sepEndBy1` eol where
    line = some cell
    cell = choice [Empty <$ ".", East <$ ">", South <$ "v"]
    withDimensions mp = Input (nbRows+1) (nbCols+1) (Map.filter (/=Empty) mp) where
        (V2 nbRows nbCols, _) = maximum (Map.toList mp)

step :: Int -> Int -> Cell -> Board -> (Board, Bool)
step nbRows nbCols direction board = (insertAll moved direction . deleteAll movable $ board, movable /= [])
    where
    movable = filter canMove . Map.keys $ Map.filter (==direction) board
    moved = map move movable
    canMove (V2 r c) = not $ p `Map.member` board where
                r' = (r + 1) `mod` nbRows
                c' = (c + 1) `mod` nbCols 
                p = if direction == East then V2 r c' else V2 r' c
    move (V2 r c) = if direction == East
                    then V2 r ((c + 1) `mod` nbCols)
                    else V2 ((r + 1) `mod` nbRows) c
    insertAll keys v mp = foldl' (\acc k -> Map.insert k v acc) mp keys
    deleteAll keys mp = foldl' (flip Map.delete) mp keys

step' :: Int -> Int -> (Board, Bool) -> (Board, Bool)
step' nbRows nbCols (board, _) = (board2, modif1 || modif2) where
    (board1, modif1) = step nbRows nbCols East board
    (board2, modif2) = step nbRows nbCols South board1

part1 :: Input -> Maybe Int
part1 (Input nbCols nbRows board) = findIndex (not . snd) $ iterate' (step' nbCols nbRows) (board, True)

solve :: Text -> IO ()
solve = aoc parser part1 (const (0 :: Int))