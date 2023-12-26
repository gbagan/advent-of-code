-- https://adventofcode.com/2015/day/12
module Day12 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Parser (Parser, alphaNumChar, some, between, signedDecimal, sepBy1)
import qualified Data.Text as Text

data Json = JInt !Int | JString !Text | JList ![Json] | JObject ![(Text, Json)] deriving (Eq)

parser :: Parser Json
parser = jint <|> jstring <|> jlist <|> jobject where
    jint = JInt <$> signedDecimal
    jstring = JString <$> string
    jlist = JList <$> between "[" "]" (parser `sepBy1` ",")
    jobject =  JObject <$> between "{" "}" (entry `sepBy1` ",")
    entry = (,) <$> string <* ":" <*> parser
    string = Text.pack <$> between "\"" "\"" (some alphaNumChar)

part1 :: Json -> Int
part1 = jsonSum where
    jsonSum (JInt n) = n
    jsonSum (JString _) = 0
    jsonSum (JList xs) = sum (map jsonSum xs) 
    jsonSum (JObject es) = sum [jsonSum v | (_,v) <- es]

part2 :: Json -> Int
part2 = jsonSum where
    jsonSum (JInt n) = n
    jsonSum (JList xs) = sum (map jsonSum xs) 
    jsonSum (JObject es) | all ((/= JString "red") . snd) es = sum [jsonSum v | (_,v) <- es]
    jsonSum _ = 0

solve :: Text -> IO ()
solve = aoc parser part1 part2
