module AOC.Prelude 
    (module P, module L, Vector, (***)) where

import Relude as P hiding (iterate, scanl, optional, many, some)
import Prelude as P (zipWith3)
import Data.List as L (elemIndex, findIndex, iterate', groupBy, partition)
import Data.Vector (Vector)
import Control.Arrow ((***))