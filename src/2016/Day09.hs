-- https://adventofcode.com/2016/day/09
module Day09 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           Text.Megaparsec (parseMaybe)
import           AOC.Parser (Parser, decimal, eol, letterChar, some, takeP, takeRest, format)
import qualified Data.Text as Text

parserWith :: (Text -> Int) -> Parser Int
parserWith f = sum <$> some (marker  <|> plain <|> 0 <$ eol) where
    plain = length <$> some letterChar
    marker = do
        (len, times) <- [format|({decimal}x{decimal})|]
        chunk <- takeP Nothing len
        pure $ times * f chunk

countChunk :: Text -> Int
countChunk chunk = part2 chunk ?: 0

part1, part2 :: Text -> Maybe Int
part1 = parseMaybe (parserWith Text.length)
part2 = parseMaybe (parserWith countChunk)

solve :: Text -> IO ()
solve = aoc takeRest part1 part2