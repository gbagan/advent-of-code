-- https://adventofcode.com/2020/day/4
module AOC2020.Day04 (solve) where
import           RIO hiding (some, try)
import qualified RIO.HashMap as Map
import           Text.Megaparsec (count, parseMaybe, sepEndBy1, some, try)
import           Text.Megaparsec.Char (alphaNumChar, char, hexDigitChar, numberChar, eol, lowerChar)
import           Text.Megaparsec.Char.Lexer (decimal)
import qualified RIO.Text as Text
import           Util (Parser, aoc)
import qualified Util as U

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

solve :: MonadIO m => Text -> m ()
solve = aoc parser (U.count checkPassport) (U.count checkPassport2)
