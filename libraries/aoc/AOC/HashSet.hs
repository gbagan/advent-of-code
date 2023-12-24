module AOC.HashSet (module Set, notMember) where

import           AOC.Prelude
import           Data.HashSet as Set

notMember :: Hashable a => a -> HashSet a -> Bool
notMember x = not . Set.member x 