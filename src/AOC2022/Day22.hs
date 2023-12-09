-- https://adventofcode.com/2022/day/22
module AOC2022.Day22 (solve) where
import           Relude hiding (head, some)
import           Relude.Unsafe ((!!), head)
import qualified Data.HashMap.Strict as Map
import           AOC.V2 (V2(..))
import           AOC (aoc)
import           AOC.Parser (Parser, sepEndBy1, some, eol, decimal)
import           AOC.Util (listTo2dMap')

type Grid = HashMap (V2 Int) Tile
data Tile = Void | Empty | Wall deriving (Eq, Show)
data Instr = L | R | Move !Int
data Input = Input !Grid ![Instr]

parser :: Parser Input
parser = Input <$> map_ <* eol <*> path where
    tile = Void <$ " " <|> Empty <$ "." <|> Wall <$ "#"
    map_ = listTo2dMap' <$> some tile `sepEndBy1` eol
    path = some instr
    instr = L <$ "L" <|> R <$ "R" <|> Move <$> decimal


move1 :: Grid -> (V2 Int, V2 Int) -> (V2 Int, V2 Int)
move1 grid (pos, dir) = (go $ modGrid (pos + dir), dir) where
    go pos' = case Map.lookup pos' grid of
        Nothing -> go $ modGrid (pos' + dir)
        Just Void -> go $ modGrid (pos' + dir)
        Just Empty -> pos'
        Just Wall -> pos
    modGrid (V2 x y) = V2 (x `mod` 200) (y `mod` 150)

move2 :: Grid -> (V2 Int, V2 Int) -> (V2 Int, V2 Int)
move2 grid (pos, dir)
    | Map.lookup pos' grid == Just Wall = (pos, dir)
    | otherwise = (pos', dir')
    where
    (dir', pos') = case (dir, pos+dir) of
        (V2 0 1, V2 r 50)  | 150 <= r -> (V2 (-1) 0, V2 149 (r-100))
        (V2 0 1, V2 r 100) | 100 <= r -> (V2 0 (-1), V2 (149-r) 149)
                           | 50 <= r  -> (V2 (-1) 0, V2 49 (50+r))
        (V2 0 1, V2 r 150)            -> (V2 0 (-1), V2 (149-r) 99)
        
        (V2 1 0, V2 200 c)            -> (V2 1 0,    V2 0 (c+100))
        (V2 1 0, V2 150 c) | 50 <= c  -> (V2 0 (-1), V2 (100+c) 49)
        (V2 1 0, V2 50 c)  | 100 <= c -> (V2 0 (-1), V2 (c-50) 99)

        (V2 0 (-1), V2 r (-1)) | r <= 149  -> (V2 0 1, V2 (149-r) 50)
                               | otherwise -> (V2 1 0, V2 0 (r-100))
        (V2 0 (-1), V2 r 49)   | r <= 49   -> (V2 0 1, V2 (149-r) 0)
                               | r <= 99   -> (V2 1 0, V2 100 (r-50))

        (V2 (-1) 0, V2 99 c)   | c <= 49   -> (V2 0 1, V2 (50+c) 50)
        (V2 (-1) 0, V2 (-1) c) | c <= 99   -> (V2 0 1, V2 (100+c) 0)
                               | otherwise -> (V2 (-1) 0, V2 199 (c-100))
        x -> x

solveWith :: (Grid -> (V2 Int, V2 Int) -> (V2 Int, V2 Int)) -> Input -> Int
solveWith move (Input grid instrs) = (r+1) * 1000 + (c+1) * 4 + dirScore finalDir where
    (V2 r c, finalDir) = foldl' go (initPosition, V2 0 1) instrs
    go (pos, dir) = \case
        L -> (pos, goLeft dir)
        R -> (pos, goRight dir)
        Move n -> iterate (move grid) (pos, dir) !! n

    initPosition = head [v | i <- [0..], let v = V2 0 i, Map.lookup v grid == Just Empty]
    goLeft (V2 dr dc) = V2 (-dc) dr
    goRight (V2 dr dc) = V2 dc (-dr)
    dirScore (V2 0 1) = 0
    dirScore (V2 1 0) = 1
    dirScore (V2 0 (-1)) = 2
    dirScore _ = 3

solve :: Text -> IO ()
solve = aoc parser (solveWith move1) (solveWith move2)