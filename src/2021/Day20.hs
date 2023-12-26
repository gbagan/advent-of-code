module Day20 (solve) where
import           AOC.Prelude hiding (fromList, toList, get)
import           Data.List ((!!))
import           Data.Massiv.Array
import           AOC (aoc)
import           AOC.Parser (Parser, sepEndBy1, char, eol, some)
import           AOC.List (count)
import           AOC.Util (binToInt)

type Algo = Array U Ix1 Bool
type Grid = Array U Ix2 Bool
data Input = Input Algo Grid

parser :: Parser Input
parser = Input <$> algo <* eol <* eol <*> grid where
    algo = fromList Seq <$> some bit
    grid = fromLists' Seq <$> some bit `sepEndBy1` eol
    bit = False <$ char '.' <|> True <$ char '#'

stencil :: Algo -> Stencil Ix2 Bool Bool
stencil algo = makeStencil (Sz2 3 3) (0 :. 0) \get -> algo ! binToInt [get (i :. j) | i <- [-1..1], j <- [-1..1]]

step :: Bool -> Algo -> Grid -> Grid
step fill algo = compute
                . dropWindow
                . applyStencil
                    (Padding (Sz2 1 1) (Sz2 3 3) (Fill fill))
                    (stencil algo)

iterateGrid :: Algo -> Grid -> [Grid]
iterateGrid algo = go (0 :: Int) where
    go n grid = grid : go (n+1) (step (odd n) algo grid) 

countLit :: Int -> Input -> Int
countLit n (Input algo grid) = count id . toList $ iterateGrid algo grid !! n

solve :: Text -> IO ()
solve = aoc parser (countLit 2) (countLit 50)