module Util where
import Data.List (genericLength)
import Data.Map (Map)
import qualified Data.Map as Map

freqs :: Ord a => [a] -> Map a Int
freqs = Map.fromListWith (+) . map (,1)

average :: [Int] -> Double
average xs = realToFrac (sum xs) / genericLength xs

majority :: (a -> Bool) -> [a] -> Bool
majority f l = 2*m >= n where
                m = length $ filter f l
                n = length l