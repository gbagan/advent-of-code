-- https://adventofcode.com/2019/day/24
module Day24 (solve) where
import           AOC.Prelude hiding (one)
import           AOC (aoc)
import           AOC.List (findDuplicate)
import           AOC.Parser (Parser, eol, some, sepEndBy1)
import           AOC.Util (times)
import           Data.Bits ((.&.), (.|.), complement, popCount, shiftL, shiftR)

parser :: Parser Word32
parser = sum . zipWith (bool 0) powers2 . concat <$> some tile `sepEndBy1` eol where
    tile = False <$ "." <|> True <$ "#"

powers2 :: [Word32]
powers2 = iterate' (*2) 1

leftFilter :: Word32
leftFilter = 0b11110_11110_11110_11110_11110

rightFilter :: Word32
rightFilter = 0b1111_01111_01111_01111_01111

sel :: Word32 -> Int -> Int -> Word32
sel input bit shift = ((input `shiftR` bit) .&. 1) `shiftL` shift
{-# INLINE sel #-}

neg :: Word32 -> Word32
neg n = complement n .&. 0x1ffffff

neighbors :: Word32 -> [Word32]
neighbors i = [i `shiftL` 5, i `shiftR` 5, (i `shiftL` 1) .&. leftFilter,  (i `shiftR` 1) .&. rightFilter]

outsideNeighbors :: Word32 -> [Word32]
outsideNeighbors i = [(sel i 7 0 + sel i 17 20) * 0b11111, (sel i 11 0 + sel i 13 4) * 0b100001000010000100001]

insideNeighbors :: Word32 -> [Word32]
insideNeighbors i = [ sel i 0 7 + sel i 0 11 + sel i 4 13 + sel i 20 17
                    , sel i 1 7 + sel i 5 11 + sel i 9 13 + sel i 21 17
                    , sel i 2 7 + sel i 10 11 + sel i 14 13 + sel i 22 17
                    , sel i 3 7 + sel i 15 11 + sel i 19 13 + sel i 23 17
                    , sel i 4 7 + sel i 20 11 + sel i 24 13 + sel i 24 17
                    ]

step :: Word32 -> [Word32] -> Word32
step layout = go 0 0 0 where
    go !one !two !three = \case
        [] -> (layout .&. one .&. neg two) .|. (neg layout .&. one .&. neg three)
        (x:xs) -> go (one .|. x) (two .|. (x .&. one)) (three .|. (x .&. two)) xs

step2 :: [Word32] -> [Word32]
step2 layouts = go layouts' where
    layouts' = 0 : 0 : layouts ++ [0, 0]
    go (outside : current : inside : xs) = current' : go (current : inside : xs) where
        current' = step current (neighbors current ++ insideNeighbors inside ++ outsideNeighbors outside) .&. neg (1 `shiftL` 12)
    go x = x

part1 :: Word32 -> Maybe Word32
part1 = findDuplicate . iterate' \l -> step l (neighbors l)

part2 :: Word32 -> Int
part2 = sum . map popCount . times 200 step2 . pure

solve :: Text -> IO ()
solve = aoc parser part1 part2