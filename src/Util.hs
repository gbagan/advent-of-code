module Util where
import           RIO
import           RIO.List (sort, genericLength)
import           RIO.List.Partial (maximum, (!!))
import qualified RIO.Map as Map
import qualified RIO.HashMap as HMap
import qualified RIO.Text as Text
import           System.CPUTime (getCPUTime)
import           Text.Megaparsec (Parsec, parse, errorBundlePretty, Token, MonadParsec)
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import           Data.Text.IO (putStrLn)

type Parser = Parsec Void Text
type BinParser = Parsec Void [Bool]
type Point = (Int, Int)

-- templates

aoc :: (MonadIO m, Show b, Show c) => Parser a -> (a -> b) -> (a -> c) -> Text -> m ()
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
        Left err -> liftIO $ putStrLn $ Text.pack $ errorBundlePretty err
        Right parsed ->
            case precomp parsed of
                Nothing -> liftIO $ putStrLn "  precomputation has failed"
                Just p -> do
                    (duration1, res1) <- duration (pure $ part1 p)
                    liftIO $ putStrLn $ "  part 1: " <> tshow res1 <> " in " <> duration1
                    (duration2, res2) <- duration (let !v = part2 p in pure v)
                    liftIO $ putStrLn $ "  part 2: " <> tshow res2  <> " in " <> duration2

-- functions on lists

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f
{-# INLINE count #-}

takeEnd :: Int -> [a] -> [a]
takeEnd n xs = drop (length xs - n) xs
{-# INLINE takeEnd #-}

freqs :: Ord a => [a] -> Map a Int
freqs = Map.fromListWith (+) . map (,1)
{-# INLINE freqs #-}

allUnique :: Ord a => [a] -> Bool
allUnique xs = length (nubOrd xs) == length xs
{-# INLINE allUnique #-}

maximumDef :: Ord a => a -> [a] -> a
maximumDef def [] = def
maximumDef _ l = maximum l

average :: [Int] -> Double
average xs = realToFrac (sum xs) / genericLength xs
{-# INLINE average #-}

majority :: (a -> Bool) -> [a] -> Bool
majority f l = 2* count f l >= length l

median :: Ord a => [a] -> a
median l = sort l !! (length l `div` 2)

cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct l1 l2 = (,) <$> l1 <*> l2
{-# INLINE cartesianProduct #-}

clamp :: Ord a => (a, a) -> a -> a
clamp (l, u) = max l . min u
{-# INLINE clamp #-}


listTo2dMap :: [[a]] -> HashMap Point a
listTo2dMap l = 
    HMap.fromList
        [((i, j), v) 
        | (j, row) <- zip [0..] l
        , (i, v) <- zip [0..] row
        ]

listTo2dMap' :: [[a]] -> Map Point a
listTo2dMap' l = 
    Map.fromList
        [((i, j), v) 
        | (j, row) <- zip [0..] l
        , (i, v) <- zip [0..] row
        ]

adjacentPoints :: Point -> [Point]
adjacentPoints (x, y) = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

kingAdjacentPoints :: Point -> [Point]
kingAdjacentPoints (x, y) = adjacentPoints (x, y) ++ [(x-1, y-1), (x+1, y-1), (x-1, y+1), (x+1, y+1)]

binToInt :: [Bool] -> Int
binToInt = foldl' (\acc x -> acc * 2 + fromEnum x) 0
{-# INLINE binToInt #-}

signedDecimal :: (MonadParsec e s m, Token s ~ Char, Num a) => m a
signedDecimal = L.decimal <|> P.char '-' *> (negate <$> L.decimal)

bitP :: Parser Bool
bitP = False <$ P.char '0' <|> True <$ P.char '1'
