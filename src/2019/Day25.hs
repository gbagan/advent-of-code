-- https://adventofcode.com/2019/day/25
module Day25 (solve) where
import           AOC.Prelude hiding (head, last, lines)
import           AOC (aoc)
import           AOC.Parser (Parser, decimal, signedDecimal, sepBy1, scanf)
import           Text.Megaparsec (Parsec, parseMaybe)
import           AOC.IntCode (Effect(..), runEffect, newMachine)
import           AOC.List (dropEnd, lastMaybe)
import           Data.List (isInfixOf, lines)
import           AOC.Graph (bfsOn)

data SearchState = SearchState 
                    { _location :: String
                    , _doors :: [String]
                    , _object :: Maybe String
                    , _path :: [String]
                    , _effect :: Effect
                    }

parser :: Parser [Int]
parser = signedDecimal `sepBy1` ","

writeInput :: String -> Effect -> Effect
writeInput [] eff = eff
writeInput (x:xs) eff = case eff of
    Input f -> writeInput xs (f (ord x))
    _ -> error "writeInput: invalid program"

readOutput :: Effect -> (String, Effect)
readOutput (Output x eff) = first (chr x :) (readOutput eff)
readOutput e = ([], e)

parseMessage :: String -> Maybe (String, [String], Maybe String)
parseMessage text = do
    let ls = lines text
    location <- drop 3 . dropEnd 3 <$> find ("==" `isPrefixOf`) ls
    let doors = map (drop 2) . takeWhile (/= "") . drop 1 $ dropWhile (/= "Doors here lead:") ls
    let object = case dropWhile (/= "Items here:") ls of
            [] -> Nothing
            [_] -> Nothing
            (_ : l : _) -> Just (drop 2 l)
    pure (location, doors, object)

oppositeDoor :: String -> String
oppositeDoor "north" = "south"
oppositeDoor "south" = "north"
oppositeDoor "west" = "east"
oppositeDoor _ = "west"

forbiddenObjects :: [String]
forbiddenObjects = ["infinite loop", "photons", "escape pod", "molten lava", "giant electromagnet"]

neighbors :: SearchState -> [SearchState]
neighbors (SearchState _ doors _ path eff) = do
    door <- doors
    let (msg, eff') = readOutput $ writeInput (door ++ "\n") eff
    (loc, doors', object) <- maybeToList $ parseMessage msg
    let doors'' | loc == "Pressure-Sensitive Floor" = []
                | otherwise = doors'
    pure $ SearchState loc doors'' object (door : path) eff'

runCommands :: Effect -> [String] -> (String, Effect)
runCommands eff cmds =
    let (msg, eff') = readOutput eff in
    case cmds of
        [] -> (msg, eff')
        (cmd:cmds') -> do
            let eff'' = writeInput (cmd++"\n") eff'
            runCommands eff'' cmds'

sublists :: [a] -> [[a]]
sublists [] = [[]]
sublists (x:xs) = sublists xs ++ map (x:) (sublists xs)

winMsgParser :: Parsec Void String Int
winMsgParser = [scanf|"Oh, hello! You should be able to get in by typing {decimal} on the keypad at the main airlock."|] 

part1 :: [Int] -> Maybe Int
part1 pgm = do
    let eff = runEffect $ newMachine pgm
    let (msg, eff') = readOutput eff
    (loc, doors, object) <- parseMessage msg
    let search = map snd $ bfsOn _location neighbors (SearchState loc doors object [] eff')
    let objsAndPaths = [ (o, path)
                       | SearchState _ _ (Just o) path _ <- search
                       , o `notElem` forbiddenObjects
                       ]
    let pathToObjects = concat [ reverse path ++ ["take " ++ o] ++ map oppositeDoor path 
                              | (o, path) <- objsAndPaths
                              ]
    let objects = map fst objsAndPaths
    (lastStep, pathToEnd) <- uncons =<< listToMaybe 
                                [ path
                                 | SearchState "Pressure-Sensitive Floor" _ _ path _ <- search
                                ]        
    let (_, eff2) = runCommands eff (pathToObjects ++ reverse pathToEnd)
    winMsg <- listToMaybe do
        objs <- sublists objects
        let (msg', _) = runCommands eff2 (map ("drop " ++) objs ++ [lastStep])
        guard . not $ "Alert!" `isInfixOf` msg'
        pure msg'
    lastLine <- lastMaybe (lines winMsg)
    parseMaybe winMsgParser lastLine

part2 :: [Int] -> Int
part2 _ = 0

solve :: Text -> IO ()
solve = aoc parser part1 part2

-- Pressure-Sensitive Floor