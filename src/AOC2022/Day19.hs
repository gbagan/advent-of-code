-- https://adventofcode.com/2022/day/1
module AOC2022.Day19 (solve) where
import           Relude
import           Data.List (maximum)
import           Lens.Micro.Extras (view)
import           Linear.V4 (V4(..), _x, _y, _z)
import           Text.Megaparsec (sepEndBy1)
import           Text.Megaparsec.Char (eol)
import           Text.Megaparsec.Char.Lexer (decimal)
import           Util (Parser, aoc)
import           Util.Search (dfsM)

type Resource = V4 Int -- ore clay obsidian geode
data Blueprint = Blueprint !Resource !Resource !Resource !Resource

parser :: Parser [Blueprint]
parser = blueprint `sepEndBy1` eol where
    blueprint = do
        _ <- "Blueprint " *> (decimal :: Parser Int) 
        ore1 <- ": Each ore robot costs " *> decimal
        ore2 <- " ore. Each clay robot costs " *> decimal
        ore3 <- " ore. Each obsidian robot costs " *> decimal
        clay3 <- " ore and " *> decimal
        ore4 <- " clay. Each geode robot costs " *> decimal
        obs4 <- " ore and " *> decimal <* " obsidian."
        pure $ Blueprint (V4 ore1 0 0 0) (V4 ore2 0 0 0) (V4 ore3 clay3 0 0) (V4 ore4 0 obs4 0)

solve' :: Int -> Blueprint -> Int
solve' totalTime (Blueprint oreRobotCost clayRobotCost obsidianRobotCost geodeRobotCost) =
    execState (dfsM nborFunc initState) 0
    where
    costs = [oreRobotCost, clayRobotCost, obsidianRobotCost, geodeRobotCost]
    maxOreCost = maximum $ map (view _x) costs
    maxClayCost = maximum $ map (view _y) costs
    maxObsidianCost = view _z geodeRobotCost
    initState = (V4 0 0 0 0, V4 1 0 0 0, totalTime)
    nborFunc (resources, robots, time) = do
        let V4 _ _ _ geodes = resources
        if time == 0 then do
            modify' (max geodes)
            pure []
        else do
            let newResources = resources + robots
            let nbors
                    -- always build a geode robot if you can
                  | all (>=0) (resources - geodeRobotCost) =
                    [(newResources - geodeRobotCost, robots + V4 0 0 0 1, time-1)]
                    -- or obsidian robot otherwise
                  | all (>=0) (resources - obsidianRobotCost) =
                    [(newResources - obsidianRobotCost, robots + V4 0 0 1 0, time-1)]
                  | otherwise =
                    (newResources, robots, time-1)
                    : [(newResources - oreRobotCost , robots + V4 1 0 0 0, time-1)
                        | all (>=0) (resources - oreRobotCost)]
                    ++ [(newResources - clayRobotCost, robots + V4 0 1 0 0, time-1)
                        | all (>=0) (resources - clayRobotCost)]
            pure . mapMaybe threshold $ nbors

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