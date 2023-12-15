{-# OPTIONS_GHC -Wno-deprecations #-}
module AOC.Debug where

import Relude

spy :: Show a => String -> a -> a
spy str x = trace (str ++ show x) x

spy' :: Show b => String -> (a -> b) -> a -> a
spy' str f x = trace (str ++ show (f x)) x