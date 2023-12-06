module Util where
import           RIO
import           RIO.List (sort, genericLength)
import           RIO.List.Partial (maximum, (!!))
import           Text.Megaparsec (Parsec, parse, errorBundlePretty)
import qualified RIO.Map as Map
import qualified RIO.HashMap as HMap
import qualified RIO.Text as Text
import           Linear.V2 (V2(..))
import           System.CPUTime (getCPUTime)
import           System.Environment (getArgs)
import           Data.Text.IO (putStrLn)

type Parser = Parsec Void Text
type Point = (Int, Int)

aocMain :: Text -> Map Text (Text -> RIO SimpleApp ()) -> IO ()
aocMain year solutions = runSimpleApp do
    args <- map Text.pack <$> liftIO getArgs
    traverse_ solveProblem if null args then Map.keys solutions else args
    where
    solveProblem name = case Map.lookup name solutions of
        Just solve -> do
            liftIO $ putStrLn $ "Solve day " <> name
            solve =<< readFileUtf8 (Text.unpack $ "./data/" <> year <> "/data" <> name)
        Nothing -> liftIO $ putStrLn $ "Day not implemented: " <> name


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
                Just !p -> do
                    (duration1, !res1) <- duration (pure $ part1 p)
                    liftIO $ putStrLn $ "  part 1: " <> tshow res1 <> " in " <> duration1
                    (duration2, !res2) <- duration (pure $ part2 p)
                    liftIO $ putStrLn $ "  part 2: " <> tshow res2  <> " in " <> duration2

-- functions on lists

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f
{-# INLINE count #-}

slice :: Int -> Int -> [a] -> [a]
slice start end = take (end - start + 1) . drop start
{-# INLINE slice #-}

freqs :: Hashable a => [a] -> HashMap a Int
freqs = HMap.fromListWith (+) . map (,1)
{-# INLINE freqs #-}

freqs' :: Ord a => [a] -> Map a Int
freqs' = Map.fromListWith (+) . map (,1)
{-# INLINE freqs' #-}

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
majority f l = 2 * count f l >= length l

median :: Ord a => [a] -> a
median l = sort l !! (length l `div` 2)

cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct l1 l2 = (,) <$> l1 <*> l2
{-# INLINE cartesianProduct #-}

clamp :: Ord a => (a, a) -> a -> a
clamp (l, u) = max l . min u
{-# INLINE clamp #-}

flattenWithIndex :: [[a]] -> [(Int, Int, a)]
flattenWithIndex l =
    [(i, j, v)
    | (i, row) <- zip [0..] l
    , (j, v) <- zip [0..] row
    ]

listTo2dMap :: [[a]] -> HashMap (Int, Int) a
listTo2dMap l =
    HMap.fromList
        [((i, j), v)
        | (j, row) <- zip [0..] l
        , (i, v) <- zip [0..] row
        ]

listTo2dMap' :: [[a]] -> HashMap (V2 Int) a
listTo2dMap' l =
    HMap.fromList
        [(V2 i j, v)
        | (i, row) <- zip [0..] l
        , (j, v) <- zip [0..] row
        ]

adjacentPoints :: Point -> [Point]
adjacentPoints (x, y) = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

kingAdjacentPoints :: Point -> [Point]
kingAdjacentPoints (x, y) = adjacentPoints (x, y) ++ [(x-1, y-1), (x+1, y-1), (x-1, y+1), (x+1, y+1)]

kingAdjacentPoints' :: V2 Int -> [V2 Int]
kingAdjacentPoints' (V2 x y) = kingAdjacentPoints (x, y) <&> uncurry V2


binToInt :: [Bool] -> Int
binToInt = foldl' (\acc x -> acc * 2 + fromEnum x) 0
{-# INLINE binToInt #-}

