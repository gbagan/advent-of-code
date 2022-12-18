module Util where
import           RIO
import           RIO.List (sort, genericLength)
import           RIO.List.Partial ((!!))
import qualified RIO.Map as Map
import qualified RIO.HashMap as HM
import qualified RIO.Text as Text
import qualified RIO.Vector as V
import           RIO.Vector ((!?))
import           RIO.Vector.Partial ((!))
import           System.CPUTime (getCPUTime)
import           Text.Megaparsec (Parsec, parse, errorBundlePretty)
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import           Data.Text.IO (putStrLn)

type Parser = Parsec Void Text
type BinParser = Parsec Void [Bool]
type Point = (Int, Int)

-- templates

aoc :: (Show b, Show c, MonadIO m) =>
        Parser a -> (a -> b) -> (a -> c) -> Text -> m ()
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
                                show diff <> " microseconds"
    pure (strDiff, res)

aoc' :: (MonadIO m, Show c, Show d) =>
        Parser a -> (a -> Maybe b) -> (b -> c) -> (b -> d) -> Text -> m ()
aoc' parser precomp part1 part2 input = do
    case parse parser "" input of
        Left err -> liftIO . putStrLn . Text.pack $ errorBundlePretty err
        Right parsed ->
            case precomp parsed of
                Nothing -> liftIO $ putStrLn "  precomputation has failed"
                Just p -> do
                    (duration1, res1) <- duration (pure $ part1 p)
                    liftIO $ putStrLn $ "  part 1: " <> tshow res1 <> " in " <> duration1
                    (duration2, res2) <- duration (let !v = part2 p in pure v)
                    liftIO $ putStrLn $ "  part 2: " <> tshow res2 <> " in " <> duration2

adjacentPoints :: Point -> [Point]
adjacentPoints (x, y) = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

kingAdjacentPoints :: Point -> [Point]
kingAdjacentPoints (x, y) = adjacentPoints (x, y) ++ [(x-1, y-1), (x+1, y-1), (x-1, y+1), (x+1, y+1)]

range :: Int -> Int -> [Int]
range x y = if x < y then [x .. y] else [y .. x]

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

freqs :: Ord a => [a] -> Map a Int
freqs = Map.fromListWith (+) . map (,1)

average :: [Int] -> Double
average xs = realToFrac (sum xs) / genericLength xs

majority :: (a -> Bool) -> [a] -> Bool
majority f l = 2* count f l >= length l

median :: Ord a => [a] -> a
median l = sort l !! (length l `div` 2)

-- matrices i.e. two dimensional arrays

type Matrix a = Vector (Vector a)

(!!!) :: Matrix a -> (Int, Int) -> a
v !!! (i, j) = v ! i ! j

(!!?) :: Matrix a -> (Int, Int) -> Maybe a
v !!? (i, j) = v !? i >>= (!?j)

flatMat :: Matrix a -> [(Int, Int, a)]
flatMat = join . zipWith (\x -> zipWith (x,,) [0..]) [0..] . map V.toList . V.toList

matFromList :: [[a]] -> Matrix a
matFromList = V.fromList . map V.fromList


binToInt :: [Bool] -> Int
binToInt = foldl' (\acc x -> acc * 2 + fromEnum x) 0

cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct l1 l2 = (,) <$> l1 <*> l2

signedInteger :: Parser Int
signedInteger = L.decimal <|> P.char '-' *> (negate <$> L.decimal)

bitP :: Parser Bool
bitP = False <$ P.char '0' <|> True <$ P.char '1'