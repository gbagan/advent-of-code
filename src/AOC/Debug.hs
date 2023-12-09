{-# OPTIONS_GHC -Wno-deprecations #-}
module AOC.Debug where

import Relude

spy :: Show a => a -> a
spy x = traceShow x x

spy' :: Show b => (a -> b) -> a -> a
spy' f x = traceShow (f x) x