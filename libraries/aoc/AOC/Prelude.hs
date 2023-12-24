module AOC.Prelude 
    (module P, module L, Vector) where

import Relude as P hiding (iterate, scanl, optional, many, some)
import Data.List as L (findIndex, iterate', partition)
import Data.Vector (Vector)