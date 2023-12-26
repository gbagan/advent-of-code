-- https://adventofcode.com/2015/day/4
module Day04 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Parser (takeRest)
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TSE
import qualified Data.ByteString.Base16 as B16 (encode)
import           Crypto.Hash.MD5 (hash)

solveFor :: ByteString -> Text -> Maybe Int
solveFor pattern text = listToMaybe
                            [i | i <- [0..]
                            , pattern `BS.isPrefixOf` B16.encode (hash (bs <> show i))
                             ] where
    bs = TSE.encodeUtf8 text

solve :: Text -> IO ()
solve = aoc takeRest (solveFor "00000") (solveFor "000000")