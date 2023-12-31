-- https://adventofcode.com/2016/day/14
module Day14 (solve) where
import           AOC.Prelude
import           Data.List ((!!))
import           AOC (aoc)
import           AOC.Parser (takeRest)
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TSE
import qualified Data.ByteString.Base16 as B16 (encode)
import qualified Data.ByteString.Char8 as C8
import           Crypto.Hash.MD5 (hash)
import           AOC.List (sliding)

hexDigits :: [Char]
hexDigits = "01234567abcdef"

generateHashes :: Text -> [(Int, ByteString)]
generateHashes text = [ (i, hashed)
                      | i <- [0..]
                      , let hashed = B16.encode (hash (bs <> show i))
                      ]
    where bs = TSE.encodeUtf8 text

-- 16448 too high

part1 :: Text -> Int
part1 text = a !! 63 where
    a = [ idx
        | ((idx, x):xs) <- sliding 1001 (generateHashes text)
        , let matched = matchedDigits 3 hexDigits x
        , not (all (null . matchedDigits 5 matched . snd) xs)
        ]

matchedDigits :: Int -> [Char] -> ByteString -> [Char]
matchedDigits n chars bs =
    [ c
    | c <- chars
    , C8.pack (replicate n c) `BS.isInfixOf` bs
    ]

part2 :: Text -> Int
part2 _ = 0

solve :: Text -> IO ()
solve = aoc takeRest part1 part2