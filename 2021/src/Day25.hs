module Day25  where
import           RIO
import           RIO.List (findIndex, iterate)
import qualified RIO.HashMap as Map
import           RIO.List.Partial (maximum)
import           Text.Megaparsec (sepEndBy1)
import           Text.Megaparsec.Char (char, eol)
import           Util (Parser, Point, aoc, listTo2dMap)

data Cell = Empty | East | South deriving (Eq, Ord)
type Board = HashMap Point Cell
data Input = Input !Int !Int !Board

parser :: Parser Input
parser = withDimensions . listTo2dMap <$> line `sepEndBy1` eol where
    line = some cell
    cell = char '.' $> Empty <|> char '>' $> East <|> char 'v' $> South
    withDimensions mp = Input (nbCols+1) (nbRows+1) (Map.filter (/=Empty) mp) where
        ((nbCols, nbRows), _) = maximum (Map.toList mp)

step :: Int -> Int -> Cell -> Board -> (Board, Bool)
step nbCols nbRows direction board = (insertAll moved direction . deleteAll movable $ board, movable /= [])
    where
    movable = filter canMove . Map.keys $ Map.filter (==direction) board
    moved = map move movable
    canMove (x, y) = not $ p `Map.member` board where
                x' = (x + 1) `mod` nbCols 
                y' = (y + 1) `mod` nbRows
                p = if direction == East then (x', y) else (x, y')
    move (x, y) = if direction == East
                    then ((x + 1) `mod` nbCols, y)
                    else (x, (y + 1) `mod` nbRows)
    insertAll keys v mp = foldl' (\acc k -> Map.insert k v acc) mp keys
    deleteAll keys mp = foldl' (flip Map.delete) mp keys

step' :: Int -> Int -> (Board, Bool) -> (Board, Bool)
step' nbCols nbRows (board, _) = (board2, modif1 || modif2) where
    (board1, modif1) = step nbCols nbRows East board
    (board2, modif2) = step nbCols nbRows South board1

part1 :: Input -> Maybe Int
part1 (Input nbCols nbRows board) = findIndex (not . snd) $ iterate (step' nbCols nbRows) (board, True)

solve :: MonadIO m => Text -> m ()
solve = aoc parser part1 (const (0 :: Int))