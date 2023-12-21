-- https://adventofcode.com/2020/day/4
module Day04 (solve) where
import           AOC.Prelude
import qualified Data.HashMap.Strict as Map
import           Text.Megaparsec (parseMaybe)
import qualified Data.Text as Text
import           AOC (aoc)
import qualified AOC.List as L
import           AOC.Parser (Parser, count, sepEndBy1, some, try, alphaNumChar, char, numberChar, eol, hexDigitChar, lowerChar, decimal)

type Passport = HashMap Text Text
data Height = Cm Int | In Int

parser :: Parser [Passport]
parser = passport `sepEndBy1` eol where
    passport = Map.fromList <$> (field `sepEndBy1` (eol <|> " "))
    field = do
        key <- Text.pack <$> some lowerChar
        _ <- char ':'
        val <- Text.pack <$> some (alphaNumChar <|> char '#')
        pure (key, val)


checkPassport :: Passport -> Bool
checkPassport pp = all (`Map.member` pp) ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

checkers :: [(Text, Text -> Bool)]
checkers =
    [   ("byr", parseAndCheck decimal (\i -> i >= (1920::Int) && i <= 2002))
    ,   ("iyr", parseAndCheck decimal (\i -> i >= (2010::Int) && i <= 2020))
    ,   ("eyr", parseAndCheck decimal (\i -> i >= (2020::Int) && i <= 2030))
    ,   ("hgt", parseAndCheck (Cm <$> try (decimal <* "cm") <|> In <$> decimal <* "in")
                            \case
                                Cm h -> h >= 150 && h <= 193
                                In h -> h >= 59 && h <= 76
        )
    ,   ("hcl", parseAndCheck (char '#' *> count 6 hexDigitChar) (const True))
    ,   ("ecl", (`elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]))
    ,   ("pid", parseAndCheck (count 9 numberChar) (const True))
    ]
    where
    parseAndCheck :: Parser a -> (a -> Bool) -> Text -> Bool
    parseAndCheck p c str = maybe False c (parseMaybe p str)

checkPassport2 :: Passport -> Bool
checkPassport2 pp = checkers & all \(key, checker) -> maybe False checker (Map.lookup key pp)

solve :: Text -> IO ()
solve = aoc parser (L.count checkPassport) (L.count checkPassport2)
