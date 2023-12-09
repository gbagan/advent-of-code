-- https://adventofcode.com/2022/day/13
module AOC2022.Day13 (solve) where
import           Relude
import           Text.Megaparsec (between, sepEndBy1, sepBy)
import           Text.Megaparsec.Char (char, eol)
import           Text.Megaparsec.Char.Lexer (decimal)
import           Util (Parser, aoc, count)

data Packet = PInt !Int | Packet ![Packet]

instance Eq Packet where
    a == b = compare a b == EQ

instance Ord Packet where
    compare (PInt n) (PInt m) = compare n m
    compare (Packet l) (Packet l') = compare l l'
    compare (PInt n) l = compare (Packet [PInt n]) l
    compare l (PInt n) = compare l (Packet [PInt n])

parser :: Parser [(Packet, Packet)]
parser = pair `sepEndBy1` (eol *> eol) where
    pair = (,) <$> packet <* eol <*> packet
    packet = PInt <$> decimal <|> listPacket
    listPacket = between (char '[') (char ']') (Packet <$> packet `sepBy` char ',')

part1 :: [(Packet, Packet)] -> Int
part1 = sum . zipWith (\i (p1, p2) -> if p1 <= p2 then i else 0) [1..]

part2 :: [(Packet, Packet)] -> Int
part2 pairs = index1 * index2 where
    packets = pairs >>= \(p1, p2) -> [p1, p2]
    index1 = 1 + count (< Packet [Packet [PInt 2]]) packets
    index2 = 2 + count (< Packet [Packet [PInt 6]]) packets

solve :: MonadIO m => Text -> m ()
solve = aoc parser part1 part2