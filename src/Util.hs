module Util where
import           RIO
import           RIO.ByteString (putStr)
import           RIO.List (group, sort, genericLength)
import           RIO.List.Partial (head, (!!))
import qualified RIO.Text as Text
import           Text.Megaparsec (Parsec, parse, errorBundlePretty)
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Map as Map

type Parser = Parsec Void Text
type BinParser = Parsec Void [Bool]
type Point = (Int, Int)

print :: (MonadIO m) => Text -> m ()
print = putStr . Text.encodeUtf8

printLn :: (MonadIO m) => Text -> m ()
printLn = print . (<> "\n") 

aocTemplate :: Parser a -> (a -> Maybe b) -> (b -> Maybe Int) -> (b -> Maybe Int) -> Text -> IO ()
aocTemplate parser precomp part1 part2 s = do
    case parse parser "" s of
        Left err -> print . Text.pack $ errorBundlePretty err
        Right input -> do
            case precomp input of
                Nothing -> printLn "precomputing failed"
                Just p -> do
                    case part1 p of
                        Nothing -> printLn "  part 1: no solution found"
                        Just n ->  printLn $ "  part 1: " <> tshow n
                    case part2 p of
                        Nothing -> printLn "  part 2: no solution found"
                        Just n ->  printLn $ "  part 2: " <> tshow n

adjacentPoints :: Point -> [Point]
adjacentPoints (x, y) = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

kingAdjacentPoints :: Point -> [Point]
kingAdjacentPoints (x, y) = adjacentPoints (x, y) ++ [(x-1, y-1), (x+1, y-1), (x-1, y+1), (x+1, y+1)]

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

freqs :: Ord a => [a] -> Map a Int
freqs = Map.fromListWith (+) . map (,1)

sortNub :: Ord a => [a] -> [a]
sortNub = map head . group . sort

average :: [Int] -> Double
average xs = realToFrac (sum xs) / genericLength xs

majority :: (a -> Bool) -> [a] -> Bool
majority f l = 2* count f l >= length l

median :: Ord a => [a] -> a
median l = sort l !! (length l `div` 2)

listTo2dMap :: [[a]] -> Map Point a
listTo2dMap l = 
    Map.fromList
        [((i, j), v) 
        | (j, row) <- zip [0..] l
        , (i, v) <- zip [0..] row
        ]

binToInt :: [Bool] -> Int
binToInt = foldl' (\acc x -> acc * 2 + fromEnum x) 0

cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct l1 l2 = (,) <$> l1 <*> l2

signedInteger :: Parser Int
signedInteger = L.decimal <|> P.char '-' *> (negate <$> L.decimal)

bitP :: Parser Bool
bitP = False <$ P.char '0' <|> True <$ P.char '1'