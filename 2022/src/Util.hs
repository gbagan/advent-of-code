module Util where
import           RIO
import           RIO.List (sort, genericLength)
import           RIO.List.Partial ((!!))
import qualified RIO.Map as Map
import qualified RIO.Text as Text
import           System.CPUTime (getCPUTime)
import           Text.Megaparsec (Parsec, parse, errorBundlePretty)
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L


type Parser = Parsec Void Text
type BinParser = Parsec Void [Bool]
type Point = (Int, Int)

-- templates

aoc :: (Show b, Show c, HasLogFunc env) =>
        Parser a -> (a -> b) -> (a -> c) -> Text -> RIO env ()
aoc parser = aoc' parser pure

duration :: MonadIO m => m a -> m (Text, a)
duration m = do
    begin <- liftIO getCPUTime
    !res <- m
    end <- liftIO getCPUTime
    let diff = (end - begin) `div` 1000000 -- in nano seconds 
    let strDiff = Text.pack $ if diff >= 10000 then
                                show (diff `div` 1000) <> " milliseconds"
                              else
                                show diff <> " nanoseconds"
    pure (strDiff, res)

aoc' :: (Show c, Show d, HasLogFunc env) =>
        Parser a -> (a -> Maybe b) -> (b -> c) -> (b -> d) -> Text -> RIO env ()
aoc' parser precomp part1 part2 input = do
    case parse parser "" input of
        Left err -> logInfo $ display $ Text.pack $ errorBundlePretty err
        Right parsed ->
            case precomp parsed of
                Nothing -> logInfo "  precomputation has failed"
                Just p -> do
                    (duration1, res1) <- duration (pure $ part1 p)
                    logInfo $ "  part 1: " <> displayShow res1 
                            <> " in " <> display duration1
                    (duration2, res2) <- duration (let !v = part2 p in pure v)
                    logInfo $ "  part 2: " <> displayShow res2 
                            <> " in " <> display duration2

-- functions on lists

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

takeEnd :: Int -> [a] -> [a]
takeEnd n xs = drop (length xs - n) xs


freqs :: Ord a => [a] -> Map a Int
freqs = Map.fromListWith (+) . map (,1)

allUnique :: Ord a => [a] -> Bool
allUnique xs = length (nubOrd xs) == length xs

average :: [Int] -> Double
average xs = realToFrac (sum xs) / genericLength xs

majority :: (a -> Bool) -> [a] -> Bool
majority f l = 2* count f l >= length l

median :: Ord a => [a] -> a
median l = sort l !! (length l `div` 2)

cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct l1 l2 = (,) <$> l1 <*> l2



clamp :: Ord a => (a, a) -> a -> a
clamp (l, u) = max l . min u

adjacentPoints :: Point -> [Point]
adjacentPoints (x, y) = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

kingAdjacentPoints :: Point -> [Point]
kingAdjacentPoints (x, y) = adjacentPoints (x, y) ++ [(x-1, y-1), (x+1, y-1), (x-1, y+1), (x+1, y+1)]

binToInt :: [Bool] -> Int
binToInt = foldl' (\acc x -> acc * 2 + fromEnum x) 0

signedDecimal :: Parser Int
signedDecimal = L.decimal <|> P.char '-' *> (negate <$> L.decimal)

bitP :: Parser Bool
bitP = False <$ P.char '0' <|> True <$ P.char '1'
