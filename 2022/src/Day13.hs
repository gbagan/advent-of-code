-- https://adventofcode.com/2022/day/13
module Day13 (solve) where
import           RIO
import           Data.List (elemIndex, sort)
import           Text.Megaparsec (between, sepEndBy1, sepBy)
import           Text.Megaparsec.Char (char, eol)
import           Text.Megaparsec.Char.Lexer (decimal)
import           Util (Parser, aoc)

data Packet = PInt Int | Packet [Packet] deriving (Eq)

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

part2 :: [(Packet, Packet)] -> Maybe Int
part2 pairs = do
    let divider1 = Packet [Packet [PInt 2]]
    let divider2 = Packet [Packet [PInt 6]]
    let packets = sort $ [divider1, divider2] ++ (pairs >>= (\(p1, p2) -> [p1, p2]))
    i1 <- elemIndex divider1 packets
    i2 <- elemIndex divider2 packets
    pure $ (i1+1) * (i2+1)

solve :: (HasLogFunc env) => Text -> RIO env ()
solve = aoc parser part1 part2