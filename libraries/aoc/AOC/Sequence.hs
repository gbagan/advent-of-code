module AOC.Sequence (module Seq, rotate) where

import           AOC.Prelude
import           Data.Sequence as Seq

rotate :: Int -> Seq a -> Seq a
rotate n s = s2 >< s1
    where (s1, s2) = Seq.splitAt (n `mod` Seq.length s) s