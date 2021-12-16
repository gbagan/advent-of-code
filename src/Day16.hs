module Day16 (solve) where

import           Data.Bifunctor (second)
import           Data.Char (digitToInt)
import           Data.List (foldl')
import           Text.Megaparsec (anySingle, count, some, takeP, parseMaybe, takeRest)
import qualified Text.Megaparsec.Char as P
import           Util (Parser, aocTemplate)

data Packet = Packet Int PacketData
data PacketData = Lit Int | Op ([Int] -> Int) [Packet]

parser :: Parser String
parser = concat <$> some (hexToBits <$> P.hexDigitChar)

hexToBits :: Char -> String
hexToBits '0' = "0000"
hexToBits '1' = "0001"
hexToBits '2' = "0010"
hexToBits '3' = "0011"
hexToBits '4' = "0100"
hexToBits '5' = "0101"
hexToBits '6' = "0110"
hexToBits '7' = "0111"
hexToBits '8' = "1000"
hexToBits '9' = "1001"
hexToBits 'A' = "1010"
hexToBits 'B' = "1011"
hexToBits 'C' = "1100"
hexToBits 'D' = "1101"
hexToBits 'E' = "1110"
hexToBits 'F' = "1111"
hexToBits _ = ""

binToInt :: String -> Int
binToInt = foldl' (\acc x -> acc * 2 + digitToInt x) 0

packet :: Parser (Int, Packet)
packet = do
    version <- binToInt <$> takeP Nothing 3
    typeid <- binToInt <$> takeP Nothing 3
    (n, dat) <- packetData typeid
    pure (n+6, Packet version dat)

packetData :: Int -> Parser (Int, PacketData)
packetData 4 = do
    chks <- chunks
    pure ((length chks * 5) `div` 4, Lit . binToInt $ chks)
packetData op = second (Op $ operator op) <$> subpackets

chunks :: Parser String
chunks = do
    b <- (=='1') <$> anySingle
    chunk <- takeP Nothing 4
    if b
        then (chunk++) <$> chunks
        else pure chunk

subpackets :: Parser (Int, [Packet])
subpackets = do
    b <- (=='1') <$> anySingle
    if b
        then do
            n <- binToInt <$> takeP Nothing 11
            (len, ps) <- unzip <$> count n packet
            pure (sum len + 12, ps)
        else do   
            len <- binToInt <$> takeP Nothing 15
            (len+16,) <$> packetsUntilLimit len
    
packetsUntilLimit :: Int -> Parser [Packet]
packetsUntilLimit n =
    if n == 0
        then pure []
        else do
            (m, p) <- packet
            (p:) <$> packetsUntilLimit (n - m)

operator :: Int -> ([Int] -> Int)
operator n = case n of
    0 -> sum
    1 -> product
    2 -> minimum
    3 -> maximum
    5 -> cmp (>)
    6 -> cmp (<)
    _ -> cmp (==)
    where cmp f (x:y:_) = fromEnum (f x y)
          cmp _ _ = 0

versionSum :: Packet -> Int
versionSum (Packet version (Lit _)) = version
versionSum (Packet version (Op _ packets)) = version + sum (map versionSum packets) 

part1 :: String -> Maybe Int
part1 s = versionSum . snd <$> parseMaybe (packet <* takeRest) s

eval :: Packet -> Int
eval (Packet _ (Lit n)) = n
eval (Packet _ (Op f packets)) = f (map eval packets)

part2 :: String -> Maybe Int
part2 s = eval . snd <$> parseMaybe (packet <* takeRest) s

solve :: String -> IO ()
solve = aocTemplate parser part1 part2