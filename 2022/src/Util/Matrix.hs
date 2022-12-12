module Util.Matrix where

import           RIO
import qualified RIO.Vector as V

-- matrices i.e. two dimensional arrays

newtype Matrix a = Matrix (Vector (Vector a))

(!?) :: Matrix a -> (Int, Int) -> Maybe a
(Matrix v) !? (i, j) = v V.!? i >>= (V.!? j)

elemsWithIndex :: Matrix a -> [(Int, Int, a)]
elemsWithIndex (Matrix m) = join . zipWith (\x -> zipWith (x,,) [0..]) [0..] . map V.toList . V.toList $ m

fromList :: [[a]] -> Matrix a
fromList = Matrix . V.fromList . map V.fromList