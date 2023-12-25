module AOC.HashMap (module Map, notMember, unionsWith) where

import           AOC.Prelude as P
import           Data.HashMap.Strict as Map

notMember :: Hashable k => k -> HashMap k v -> Bool
notMember x = not . Map.member x 

unionsWith :: Hashable a => (b -> b -> b) -> [HashMap a b] -> HashMap a b
unionsWith f = P.foldl' (Map.unionWith f) Map.empty
