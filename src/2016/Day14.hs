-- https://adventofcode.com/2016/day/14
module Day14 (solve) where
import           AOC.Prelude
import           Data.List ((!!), isInfixOf)
import           AOC (aoc)
import           AOC.Parser (takeRest)
import qualified Data.Text.Encoding as TSE
import qualified Data.ByteString.Base16 as B16 (encode)
import qualified Data.ByteString.Char8 as C8
import           Crypto.Hash.MD5 (hash)
import           AOC.List (sliding)
import           AOC.Util (times)

generateHashes :: Int -> Text -> [(Int, String)]
generateHashes n text = [ (i, C8.unpack hashed)
                        | i <- [0..]
                        , let hashed = times n (B16.encode . hash) (bs <> show i)
                        ]
    where bs = TSE.encodeUtf8 text

find3repetitions :: String -> Maybe Char
find3repetitions (x:y:z:xs) | x == y && y == z = Just x
                            | otherwise = find3repetitions (y:z:xs)
find3repetitions _ = Nothing

has5repetitions :: Char -> String -> Bool
has5repetitions c str = replicate 5 c `isInfixOf` str

solveFor :: Int -> Text -> Int
solveFor n text = a !! 63 where
    a = [ idx
        | ((idx, x):xs) <- sliding 1001 (generateHashes n text)
        , case find3repetitions x of
            Nothing -> False
            Just c -> any (has5repetitions c . snd) xs
        ]

solve :: Text -> IO ()
solve = aoc takeRest (solveFor 1) (solveFor 2017)