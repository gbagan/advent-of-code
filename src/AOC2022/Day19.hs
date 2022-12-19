-- https://adventofcode.com/2022/day/1
module AOC2022.Day19 (solve) where
import           RIO
import           RIO.List.Partial (maximum)
import           RIO.State (execState, modify')
import           Linear.V4 (V4(..), _x, _y, _z)
import           Text.Megaparsec (sepBy1, sepEndBy1)
import           Text.Megaparsec.Char (char, eol, string)
import           Text.Megaparsec.Char.Lexer (decimal)
import           Util (Parser, aoc)
import           Util.Search (dfsM)

type Ressource = V4 Int -- ore clay obsidian geode
data Blueprint = Blueprint !Ressource !Ressource !Ressource !Ressource

parser :: Parser [Blueprint]
parser = blueprint `sepEndBy1` eol where
    blueprint = do
        _ <- string "Blueprint " *> (decimal :: Parser Int) *> string ": "
        oreRobot <- robot "ore"
        _ <- char ' '
        clayRobot <- robot "clay"
        _ <- char ' '
        obsidianRobot <- robot "obsidian"
        _ <- char ' '
        geodeRobot <- robot "geode"
        pure $ Blueprint oreRobot clayRobot obsidianRobot geodeRobot

    robot :: Text -> Parser Ressource
    robot r = string ("Each " <> r <> " robot costs " ) *> (sum <$> (ore `sepBy1` string " and ")) <* char '.'
    ore = oreFunc <$> (decimal <* char ' ') <*> (string "ore" <|> string "clay"  <|> string "obsidian")
    oreFunc d "ore" = V4 d 0 0 0
    oreFunc d "clay" = V4 0 d 0 0
    oreFunc d _ = V4 0 0 d 0

solve' :: Int -> Blueprint -> Int
solve' totalTime (Blueprint oreRobotCost clayRobotCost obsidianRobotCost geodeRobotCost) =
    execState (dfsM nborFunc initState) 0
    where
    costs = [oreRobotCost, clayRobotCost, obsidianRobotCost, geodeRobotCost]
    maxOreCost = maximum $ map (view _x) costs
    maxClayCost = maximum $ map (view _y) costs
    maxObsidianCost = view _z geodeRobotCost
    initState = (V4 0 0 0 0, V4 1 0 0 0, totalTime)
    nborFunc (ressources, robots, time) = do
        let V4 _ _ _ geodes = ressources
        if time == 0 then do
            modify' (max geodes)
            pure []
        else do
            let newRessources = ressources + robots
            pure . mapMaybe threshold $ 
                [(newRessources, robots, time-1)]
                ++ [(newRessources - oreRobotCost , robots + V4 1 0 0 0, time-1)
                    | all (>=0) (ressources - oreRobotCost)]
                ++ [(newRessources - clayRobotCost, robots + V4 0 1 0 0, time-1)
                    | all (>=0) (ressources - clayRobotCost)]
                ++ [(newRessources - obsidianRobotCost, robots + V4 0 0 1 0, time-1) 
                    | all (>=0) (ressources - obsidianRobotCost)]
                ++ [(newRessources - geodeRobotCost, robots + V4 0 0 0 1, time-1)
                    | all (>=0) (ressources - geodeRobotCost)]
    threshold (V4 ore clay obsidian geode, robots@(V4 oreRobots clayRobots obsidianRobots _), time) =
        if oreRobots > maxOreCost || clayRobots > maxClayCost then
            Nothing
        else
            Just (V4 (min ore (time*maxOreCost-oreRobots*(time-1)))
                (min clay (time*maxClayCost-clayRobots*(time-1)))
                (min obsidian (time*maxObsidianCost-obsidianRobots*(time-1)))
                geode
                , robots
                , time
                )

part1 :: [Blueprint] -> Int
part1 = sum . zipWith (*) [1..] . map (solve' 24)

part2 :: [Blueprint] -> Int
part2 = product . map (solve' 32) . take 3

solve :: MonadIO m => Text -> m ()
solve = aoc parser part1 part2