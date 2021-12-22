module Day10 (solve) where
import           RIO hiding (some)
import           Text.Megaparsec (sepEndBy1, some)
import qualified Text.Megaparsec.Char as P
import           Util (Parser, aocTemplate, median)

parser :: Parser [String]
parser = line `sepEndBy1` P.eol where
        line = some $ P.char '(' <|> P.char '[' <|> P.char '{' <|> P.char '<'
                     <|> P.char ')' <|> P.char ']' <|> P.char '}' <|> P.char '>'

parseLine :: String -> Either Char [Char]
parseLine = go [] where
    go stack [] = Right stack
    go stack  (x:xs) | x `elem` ("([{<" :: String)            = go (x:stack) xs
    go (s:ss) (x:xs) | [s, x] `elem` ["()", "[]", "{}", "<>"] = go ss xs
    go _ (x:_) = Left x

part1 :: [String] -> Int
part1 = sum . map weight . lefts . map parseLine where
        weight ')' = 3
        weight ']' = 57
        weight '}' = 1197
        weight '>' = 25137
        weight _ = 0

part2 :: [String] -> Int 
part2 = median . map stackWeight . rights . map parseLine where
        weight '(' = 1
        weight '[' = 2
        weight '{' = 3
        weight '<' = 4
        weight _ = 0
        stackWeight = foldl' (\acc x -> acc * 5 + weight x) 0

solve :: Text -> IO ()
solve = aocTemplate parser pure (pure . part1) (pure . part2)