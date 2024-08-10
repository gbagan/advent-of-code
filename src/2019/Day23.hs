-- https://adventofcode.com/2019/day/23
module Day23 (solve) where
import           AOC.Prelude
import           AOC (aoc')
import           AOC.Parser (Parser, signedDecimal, sepBy1)
import           AOC.List (findDuplicate, headMaybe)
import           AOC.IntCode (Effect(..), runEffect, newMachine)
import qualified Data.IntMap.Strict as Map
import           Data.Sequence (Seq(..), (><))
import qualified Data.Sequence as Seq

data Packet = Packet !Int !Int !Int deriving (Show)

parser :: Parser [Int]
parser = signedDecimal `sepBy1` ","

simulate :: [Int] -> [(Packet, Bool)]
simulate pgm = go effs queue Nothing where
    eff = runEffect $ newMachine pgm
    effs = Map.fromList [(i, eff) | i <- [0..49]]
    queue = Seq.fromList [Packet i i (-1) | i <- [0..49]]

go :: IntMap Effect -> Seq Packet -> Maybe Packet -> [(Packet, Bool)]
go effs (packet :<| queue) natPacket = (packet, False) : deliver effs queue natPacket packet
go effs Seq.Empty (Just natPacket)   = (natPacket, True) : deliver effs Seq.empty (Just natPacket) natPacket
go _ _ _                             = error "go: empty queue and no packet stored by NAT"

deliver :: IntMap Effect -> Seq Packet -> Maybe Packet -> Packet ->  [(Packet, Bool)]
deliver effs queue natPacket (Packet d x y)
    | d == 255  = go effs queue (Just (Packet 0 x y))
    | otherwise = go effs' queue' natPacket
    where
    (packets, eff') = readPackets $ writeInput [x, y] (effs Map.! d)
    effs' = Map.insert d eff' effs
    queue' = queue >< Seq.fromList packets

writeInput :: [Int] -> Effect -> Effect
writeInput [] eff = eff
writeInput (x:xs) eff = case eff of
    Input f -> writeInput xs (f x)
    _ -> error "writeInput: invalid program"

readPackets :: Effect -> ([Packet], Effect)
readPackets (Output d (Output x (Output y eff))) = first (Packet d x y :) (readPackets eff)
readPackets e = ([], e)

part1, part2 :: [(Packet, Bool)] -> Maybe Int
part1 packets = headMaybe [y | (Packet 255 _ y, _) <- packets]
part2 packets = findDuplicate [y | (Packet 0 _ y, True) <- packets]

solve :: Text -> IO ()
solve = aoc' parser (Just . simulate) part1 part2