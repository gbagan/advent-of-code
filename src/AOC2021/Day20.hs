module AOC2021.Day20 (solve) where
import           Relude hiding (get, some)
import           Relude.Unsafe ((!!))
import qualified Data.Massiv.Array as A
import           Text.Megaparsec (sepEndBy1, some)
import           Text.Megaparsec.Char (char, eol)
import           Util (Parser, aoc, binToInt, count)

type Algo = A.Array A.U A.Ix1 Bool
type Grid = A.Array A.U A.Ix2 Bool
data Input = Input Algo Grid

parser :: Parser Input
parser = Input <$> algo <* eol <* eol <*> grid where
    algo = A.fromList A.Seq <$> some bit
    grid = A.fromLists' A.Seq <$> some bit `sepEndBy1` eol
    bit = False <$ char '.' <|> True <$ char '#'

stencil :: Algo -> A.Stencil A.Ix2 Bool Bool
stencil algo = A.makeStencil (A.Sz2 3 3) (0 A.:. 0) \get -> algo A.! binToInt [get (i A.:. j) | i <- [-1..1], j <- [-1..1]]

step :: Bool -> Algo -> Grid -> Grid
step fill algo = A.compute
                . A.dropWindow
                . A.applyStencil
                    (A.Padding (A.Sz2 1 1) (A.Sz2 3 3) (A.Fill fill))
                    (stencil algo) 

iterateGrid :: Algo -> Grid -> [Grid]
iterateGrid algo = go (0 :: Int) where
    go n grid = grid : go (n+1) (step (odd n) algo grid) 

countLit :: Int -> Input -> Int
countLit n (Input algo grid) = count id . A.toList $ iterateGrid algo grid !! n

solve :: MonadIO m => Text -> m ()
solve = aoc parser (countLit 2) (countLit 50)