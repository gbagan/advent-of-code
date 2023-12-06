module AOC2021.Day16 (solve) where
import           RIO hiding (some)
import           RIO.List (unzip)
import           RIO.List.Partial (maximum, minimum)
import           Text.Megaparsec (anySingle, count, some, takeP, parseMaybe, takeRest)
import           Text.Megaparsec.Char (hexDigitChar)
import           Util (Parser, aoc', binToInt)
import           Util.Parser (BinParser)

data Packet = Packet Int PacketData
data PacketData = Lit Int | Op ([Int] -> Int) [Packet]

parser :: Parser [Bool]
parser = concat <$> some (hexToBits <$> hexDigitChar)

hexToBits :: Char -> [Bool]
hexToBits = map (=='1') . \case {
    '0' -> "0000"; '1' -> "0001"; '2' -> "0010"; '3' -> "0011";
    '4' -> "0100"; '5' -> "0101"; '6' -> "0110"; '7' -> "0111";
    '8' -> "1000"; '9' -> "1001"; 'A' -> "1010"; 'B' -> "1011";
    'C' -> "1100"; 'D' -> "1101"; 'E' -> "1110"; 'F' -> "1111";
    _ -> ""
}

packet :: BinParser (Int, Packet)
packet = do
    version <- binToInt <$> takeP Nothing 3
    typeid <- binToInt <$> takeP Nothing 3
    (n, dat) <- packetData typeid
    pure (n+6, Packet version dat)

packetData :: Int -> BinParser (Int, PacketData)
packetData 4 = do
    chks <- chunks
    pure ((length chks * 5) `div` 4, Lit . binToInt $ chks)
packetData op = second (Op $ operator op) <$> subpackets

chunks :: BinParser [Bool]
chunks = do
    b <- anySingle
    chunk <- takeP Nothing 4
    if b
        then (chunk++) <$> chunks
        else pure chunk

subpackets :: BinParser (Int, [Packet])
subpackets = do
    b <- anySingle
    if b
        then do
            n <- binToInt <$> takeP Nothing 11
            (len, ps) <- unzip <$> count n packet
            pure (sum len + 12, ps)
        else do   
            len <- binToInt <$> takeP Nothing 15
            (len+16,) <$> packetsUntilLimit len
    
packetsUntilLimit :: Int -> BinParser [Packet]
packetsUntilLimit n =
    if n == 0
        then pure []
        else do
            (len, p) <- packet
            (p:) <$> packetsUntilLimit (n - len)

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

precomp :: [Bool] -> Maybe Packet
precomp = parseMaybe (snd <$> packet <* takeRest)

part1 :: Packet -> Int
part1 (Packet version (Lit _)) = version
part1 (Packet version (Op _ packets)) = version + sum (map part1 packets) 

part2 :: Packet -> Int
part2 (Packet _ (Lit n)) = n
part2 (Packet _ (Op f packets)) = f (map part2 packets)

solve :: MonadIO m => Text -> m ()
solve = aoc' parser precomp part1 part2