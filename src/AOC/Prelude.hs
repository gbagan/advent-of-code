module AOC.Prelude 
    (module P, Vector, findIndex, iterate', partition) where

import Relude as P hiding (iterate, scanl, optional, many, some)
import Data.List (findIndex, iterate', partition)
import Data.Vector (Vector)