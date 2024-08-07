module AOC.MassivArray (module A, (//)) where

import           AOC.Prelude
import           Data.Massiv.Array as A

(//) :: (Manifest r a, Index ix) => A.Array r ix a -> [(ix, a)] -> A.Array r ix a
a // xs = withMArrayST_ a \ma ->
    for_ xs \(ix, v) ->
        write_ ma ix v