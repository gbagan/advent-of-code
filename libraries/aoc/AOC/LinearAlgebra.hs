module AOC.LinearAlgebra where
import           AOC.Prelude hiding (head, init, last)
import           Data.Maybe (fromJust)
import           Data.List (head, init, last)
import           AOC.Monad (findM)
import           Control.Monad (foldM)
import           Control.Monad.ST (ST, runST)
import           Data.Massiv.Array (Matrix, MArray, B, Ix2, Comp(Seq), Sz(..), Ix2(..), 
                                    fromLists', toLists, size, thawS, readM, modify_, swapM, sizeOfMArray)
import           Data.Massiv.Array.Unsafe (unsafeFreeze)

gaussJordanStep :: (Eq a, Fractional a) => MArray s B Ix2 a -> (Int, a) -> Int -> ST s (Int, a)
gaussJordanStep mat (pivot, det) j = do
    let Sz2 h w = sizeOfMArray mat
    mbk <- findM (\i -> (/= 0) <$> readM mat (Ix2 i j)) [pivot..h-1]
    case mbk of
        Nothing -> pure (pivot, 0)
        Just k -> do
            v <- readM mat (Ix2 k j)
            for_ [0..w-1] \j' -> do
                modify_ mat (pure . (/ v)) (Ix2 k j')
                swapM mat (Ix2 k j') (Ix2 pivot j')
            for_ [0..h-1] \i -> do
                v1 <- readM mat (Ix2 i j)
                for_ [0..w-1] \j' -> do 
                    unless (i == pivot) do
                        modify_ mat (\v' -> do
                                v2 <- readM mat (Ix2 pivot j')
                                pure $ v' - v1 * v2
                            ) (Ix2 i j')
            pure (pivot + 1, det * v * (if pivot == k then 1 else -1))

{-# INLINE gaussJordanStep #-}

gaussJordan :: (Eq a, Fractional a) => Matrix B a -> (Matrix B a, a)
gaussJordan m = runST do
    let Sz2 _ w = size m
    mat <- thawS m
    (_, det) <- foldM (gaussJordanStep mat) (0, 1) [0..w-1]
    m' <- unsafeFreeze Seq mat
    pure (m', det)

{-# INLINE gaussJordan #-}

solveLinearSystem :: (Eq a, Fractional a) => [[a]] -> Maybe [a]
solveLinearSystem mat =
    if all (==0) (init (last echelon)) then
        Nothing
    else
        let indicesAndValues =
                map (\row -> (fromJust (elemIndex 1 row), last row)) echelon
                -- search, for each row, the column of the leading one.
                -- It always exists and the sequence is increasing
        in Just (getSolution indicesAndValues 0 (length (head mat)))
    where
    echelon = removeZeroRows . toLists . fst . gaussJordan $ fromLists' Seq mat
    removeZeroRows = filter (any (/=0))
    getSolution _ i n           | i == n = []
    getSolution ((j, v):xs) i n | j == i = v : getSolution xs (i+1) n
    getSolution xs i n                   = 0 : getSolution xs (i+1) n

{-# INLINE solveLinearSystem #-}