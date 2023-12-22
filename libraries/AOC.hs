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
    solveProblem name = case Map.lookup name solutions of
        Just solve -> do
            putStrLn $ "Solve day " <> name
            bytes <- readFileBS ("./data/" ++ year ++ "/data" ++ name)
            solve $ decodeASCII bytes
        Nothing -> putStrLn $ "Day not implemented: " <> name


-- templates

aoc :: (Show b, Show c) => Parser a -> (a -> b) -> (a -> c) -> Text -> IO ()
aoc parser = aoc' parser pure

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

aoc' :: (Show c, Show d) =>
        Parser a -> (a -> Maybe b) -> (b -> c) -> (b -> d) -> Text -> IO ()
aoc' parser precomp part1 part2 input = do
    case parse parser "" input of
        Left err -> putStrLn $ errorBundlePretty err
        Right parsed ->
            case precomp parsed of
                Nothing -> putStrLn "  precomputation has failed"
                Just !p -> do
                    (duration1, !res1) <- duration (pure $ part1 p)
                    putStrLn $ "  part 1: " ++ show res1 <> " in " ++ duration1
                    (duration2, !res2) <- duration (pure $ part2 p)
                    putStrLn $ "  part 2: " ++ show res2  <> " in " ++ duration2