-- https://adventofcode.com/2023/day/22
module Day22 (solve) where
import           AOC.Prelude
import           Data.List (maximum)
import           AOC (aoc')
import qualified Data.HashMap.Strict as HMap
import qualified Data.IntMap.Strict as Map
import qualified Data.HashSet as HSet
import           AOC.Parser (Parser, decimal, eol, sepEndBy1)
import           AOC.V2 (V2(..))
import           AOC.V3 (V3(..), _z)
import           Lens.Micro.Extras (view)

data Brick = Brick { _begin :: !(V3 Int), _end :: !(V3 Int) } deriving (Show)
type Cube = V3 Int
type Space = HashMap (V2 Int) Int

parser :: Parser [Brick]
parser = brick `sepEndBy1` eol where
    brick = Brick <$> coord <* "~" <*> coord
    coord = V3 <$> decimal <* "," <*> decimal <* "," <*> decimal

sortBricks :: [Brick] -> [Brick]
sortBricks = sortOn (view _z . _begin) 
            . map (\(Brick p1 p2) -> Brick (min p1 p2) (max p1 p2))

cubesOf :: Brick -> [Cube]
cubesOf (Brick (V3 x1 y1 z1) (V3 x2 y2 z2)) = V3 <$> [x1..x2] <*> [y1..y2] <*> [z1,z2]

fallOne :: (Space, Brick) -> (Space, Brick)
fallOne (space, brick) = (space', brick') where
    cubes = cubesOf brick
    height = maximum [HMap.findWithDefault 0 (V2 x y) space | (V3 x y _) <- cubes]
    Brick start@(V3 _ _ z) end = brick
    brick' = Brick (start - V3 0 0 (z - height)) (end - V3 0 0 (z - height)) 
    space' = foldl' go space (cubesOf brick')
    go spc (V3 x y z') = HMap.insert (V2 x y) (z'+1) spc

fall :: [Brick] -> [Brick]
fall = go HMap.empty where
    go _ [] = []
    go space (brick:bricks) = brick' : go space' bricks where
        (space', brick') = fallOne (space, brick) 

cubeOwners :: [Brick] -> HashMap (V3 Int) Int
cubeOwners = foldl' go HMap.empty . zip [0..] where
    go owners (i, brick) = foldl' 
                            (\owners' cube -> HMap.insert cube i owners')
                            owners
                            (cubesOf brick)

precomp :: [Brick] -> ([Brick], IntMap [Int], IntMap [Int])
precomp bricks = (bricks', support, supported) where
    bricks' = fall (sortBricks bricks)
    owners = cubeOwners bricks'
    supportOf i brick =
        if view _z (_begin brick) == 0
            then [-1]
            else ordNub .catMaybes $ 
                [  j
                | cube <- cubesOf brick
                , let j = owners HMap.!? (cube - V3 0 0 1)
                , j /= Just i
                ]
    support = Map.fromList [(i, supportOf i brick) | (i, brick) <- zip [0..] bricks']
    supportedBy i brick =
        ordNub . catMaybes $ [ j 
                             | cube <- cubesOf brick
                             , let j = owners HMap.!? (cube + V3 0 0 1)
                             , Just i /= j
                             ]
    supported = Map.fromList [(i, supportedBy i brick) | (i, brick) <- zip [0..] bricks']

part1 :: ([Brick], IntMap [Int], IntMap [Int]) -> Int
part1 (bricks, support, supported) = length disintegrated where
    isStable i = length (support Map.! i) >= 2
    canBeDisintegrated i = all isStable (supported Map.! i)
    disintegrated = [i | (i, _) <- zip [0..] bricks, canBeDisintegrated i]

dfs :: Hashable a => (a -> HashSet a -> [a]) -> a -> HashSet a
dfs nborFunc start = go HSet.empty [start] where
    go visited [] = visited
    go visited (v:queue)
        | v `HSet.member` visited = go visited queue
        | otherwise =
            let visited' = HSet.insert v visited
                nbors = nborFunc v visited' 
            in go visited' (nbors ++ queue)

part2 :: ([Brick], IntMap [Int], IntMap [Int]) -> Int
part2 (bricks, support, supported) = res where
    nborFunc v disintegrated =
        [ next 
        | next <- supported Map.! v
        , all (`HSet.member` disintegrated) (support Map.! next)
        ] 
    res = sum [HSet.size (dfs nborFunc i) - 1 | (i, _) <- zip [0..] bricks]

solve :: Text -> IO ()
solve = aoc' parser (Just . precomp) part1 part2