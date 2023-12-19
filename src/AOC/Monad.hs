module AOC.Monad where

import AOC.Prelude

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM m t f = do
    b <- m
    if b then t else f

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM _ [] = pure Nothing
findM p (x:xs) = do
    b <- p x
    if b then pure (Just x) else findM p xs 