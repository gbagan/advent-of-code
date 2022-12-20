module Util.Matrix where

import           RIO
import qualified RIO.Vector as V
import qualified RIO.Vector.Partial as V ((!))

-- matrices i.e. two dimensional arrays

newtype Matrix a = Matrix (Vector (Vector a)) deriving (Eq)

nbRows :: Matrix a -> Int
nbRows (Matrix m) = V.length m

nbColumns :: Matrix a -> Int
nbColumns (Matrix m) = V.length (m V.! 0)

(!) :: Matrix a -> (Int, Int) -> a
(Matrix v) ! (i, j) = v V.! i V.! j

(!?) :: Matrix a -> (Int, Int) -> Maybe a
(Matrix v) !? (i, j) = v V.!? i >>= (V.!? j)

elems :: Matrix a -> [a]
elems (Matrix m) = concatMap V.toList . V.toList $ m

elemsWithIndex :: Matrix a -> [(Int, Int, a)]
elemsWithIndex (Matrix m) = join . zipWith (\x -> zipWith (x,,) [0..]) [0..] . map V.toList . V.toList $ m

fromList :: [[a]] -> Matrix a
fromList = Matrix . V.fromList . map V.fromList

generate :: Int -> Int -> (Int -> Int -> a) -> Matrix a
generate n m f = Matrix $ V.generate n \i -> V.generate m (f i)