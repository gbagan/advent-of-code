module Day25  where
import           RIO
import           RIO.List (findIndex, iterate)
import qualified RIO.Map as Map
import           RIO.Map.Partial (findMax)
import           Text.Megaparsec (sepEndBy1)
import qualified Text.Megaparsec.Char as P
import           Util (Parser, Point, aocTemplate, listTo2dMap)

data Cell = Empty | East | South deriving (Eq)
type Board = Map Point Cell

parser :: Parser (Int, Int, Board)
parser = withDimensions . listTo2dMap <$> line `sepEndBy1` P.eol where
    line = some cell
    cell = P.char '.' $> Empty <|> P.char '>' $> East <|> P.char 'v' $> South
    withDimensions mp = (nbCols+1, nbRows+1, Map.filter (/=Empty) mp) where
        ((nbCols, nbRows), _) = findMax mp

step :: Int -> Int -> Cell -> Board -> (Board, Bool)
step nbCols nbRows direction board = (insertAll moved direction . deleteAll movable $ board, movable /= [])
    where
    movable = filter canMove . Map.keys $ Map.filter (==direction) board
    moved = map move movable
    canMove (x, y) = p `Map.notMember` board where
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

part1 :: (Int, Int, Board) -> Maybe Int
part1 (nbCols, nbRows, board) = findIndex (not . snd) $ iterate (step' nbCols nbRows) (board, True)

solve :: (HasLogFunc env) => Text -> RIO env ()
solve = aocTemplate parser pure part1 (pure . const 0)