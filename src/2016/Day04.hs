-- https://adventofcode.com/2016/day/4
module Day04 (solve) where
import           AOC.Prelude hiding (unwords)
import           AOC (aoc)
import           AOC.Parser (Parser, decimal, eol, sepEndBy1, some, lowerChar, scanf)
import           AOC.List (freqs, headMaybe, isInfixOf, unwords)

data Room = Room { _name :: ![String], sectorId :: !Int,  _checksum :: !String }

parser :: Parser [Room]
parser = room `sepEndBy1` eol where
    room = [scanf|$Room {name}{decimal}[{checksum}]|]
    name = some lowerChar `sepEndBy1` "-"
    checksum = some lowerChar

part1 :: [Room] -> Int
part1 rooms = sum [room.sectorId | room <- rooms, isRealRoom room] where
    isRealRoom (Room name _ checksum) = (take 5 . map fst . sortOn order . freqs . concat) name == checksum
    order (c, f) = (-f, c)

decrypt :: Room -> String
decrypt (Room name id_ _) =  unwords $ map (map decryptChar) name where
    decryptChar c = chr $ ord 'a' + ((ord c - ord 'a' + id_) `rem` 26)

part2 :: [Room] -> Maybe Int
part2 rooms = headMaybe [room.sectorId | room <- rooms, "north" `isInfixOf` decrypt room]

solve :: Text -> IO ()
solve = aoc parser part1 part2