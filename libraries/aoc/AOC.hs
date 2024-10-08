module AOC where
import           AOC.Prelude
import           Text.Megaparsec (parse, errorBundlePretty)
import qualified Data.Map.Strict as Map
import           System.CPUTime (getCPUTime)
import           AOC.Parser (Parser)
import           Data.Text.Encoding (decodeASCII)

aocMain :: String -> Map String (Text -> IO ()) -> IO ()
aocMain year solutions = do
    args <- getArgs
    traverse_ solveProblem if null args then Map.keys solutions else args
    where
    solveProblem day = case Map.lookup day solutions of
        Just solve -> do
            putStrLn $ "Solve day " <> day
            bytes <- readFileBS ("./inputs/" ++ year ++ "/" ++ day)
            solve $ decodeASCII bytes
        Nothing -> putStrLn $ "Day not implemented: " <> day

-- templates

aoc :: (Show b, Show c) => Parser a -> (a -> b) -> (a -> c) -> Text -> IO ()
aoc parser = aoc' parser pure

aoc' :: (Show c, Show d) =>
        Parser a -> (a -> Maybe b) -> (b -> c) -> (b -> d) -> Text -> IO ()
aoc' parser precomp part1 part2 = aocIO' parser precomp (pure . part1) (pure . part2)

aocIO :: (Show b, Show c) => Parser a -> (a -> IO b) -> (a -> IO c) -> Text -> IO ()
aocIO parser = aocIO' parser pure

aocIO' :: (Show c, Show d) =>
        Parser a -> (a -> Maybe b) -> (b -> IO c) -> (b -> IO d) -> Text -> IO ()
aocIO' parser precomp part1 part2 input = do
    case parse parser "" input of
        Left err -> putStrLn $ errorBundlePretty err
        Right parsed ->
            case precomp parsed of
                Nothing -> putStrLn "  precomputation has failed"
                Just !p -> do
                    (duration1, !res1) <- duration (part1 p)
                    putStrLn $ "  part 1: " ++ show res1 <> " in " ++ duration1
                    (duration2, !res2) <- duration (part2 p)
                    putStrLn $ "  part 2: " ++ show res2  <> " in " ++ duration2

aoc_ :: (NFData b, NFData c, Show b, Show c) =>
        Parser a -> (a -> Maybe (b, c)) -> Text -> IO ()
aoc_ parser f = aocIO_ parser (pure . f) 

aocIO_ :: (NFData b, NFData c, Show b, Show c) =>
        Parser a -> (a -> IO (Maybe (b, c))) -> Text -> IO ()
aocIO_ parser f input = do
    case parse parser "" input of
        Left err -> putStrLn $ errorBundlePretty err
        Right parsed -> do
                    (duration1, !mres) <- duration do
                        a <- f parsed
                        pure $ a `deepseq` a
                    case mres of
                        Nothing -> putStrLn "  program has failed"
                        Just (res1, res2) -> do
                            putStrLn $ "  part 1: " ++ show res1
                            putStrLn $ "  part 2: " ++ show res2  <> ", total duration:  " ++ duration1

duration :: IO a -> IO (String, a)
duration m = do
    begin <- getCPUTime
    !res <- m
    end <- getCPUTime
    let diff = (end - begin) `div` 1_000_000 -- in microseconds 
    let strDiff = if diff >= 10000 then
                    show (diff `div` 1000) <> " milliseconds"
                  else
                    show diff <> " microseconds"
    pure (strDiff, res)