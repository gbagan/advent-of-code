-- https://adventofcode.com/2016/day/5
module Day05 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Parser (takeRest)
import qualified Data.Map.Strict as Map 
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TSE
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Base16 as B16 (encode)
import           Crypto.Hash.MD5 (hash)

generateHashes :: Text -> [ByteString]
generateHashes text = [ hashed
                      | i <- [(0::Int)..]
                      , let hashed = B16.encode (hash (bs <> show i))
                      , "00000" `BS.isPrefixOf` hashed
                      ]
    where bs = TSE.encodeUtf8 text      

part1 :: Text -> String
part1 = take 8 . map (`C8.index` 5) . generateHashes

part2 :: Text -> Maybe String
part2 = fmap Map.elems . find (\m -> Map.size m == 8) . scanl' go Map.empty . generateHashes
    where
    go m bs = if idx <= '7' && idx `Map.notMember` m
                then Map.insert idx (C8.index bs 6) m
                else m
        where idx = C8.index bs 5

solve :: Text -> IO ()
solve = aoc takeRest part1 part2