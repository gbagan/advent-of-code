-- https://adventofcode.com/2023/day/22
module Day22 (solve) where
import           AOC.Prelude
import           Data.List (maximum)
import           AOC (aoc')
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import qualified Data.Vector as V
import           AOC.Parser (Parser, decimal, eol, sepEndBy1)
import           AOC.V2 (V2(..))
import           AOC.V3 (V3(..), _z)
import           Lens.Micro.Extras (view)
import           AOC.Graph (reachableFrom')

data Brick = Brick { _begin :: !(V3 Int), _end :: !(V3 Int) }
type Cube = V3 Int
type Heights = HashMap (V2 Int) Int

parser :: Parser [Brick]
parser = brick `sepEndBy1` eol where
    brick = Brick <$> coord <* "~" <*> coord
    coord = V3 <$> decimal <* "," <*> decimal <* "," <*> decimal

sortBricks :: [Brick] -> [Brick]
sortBricks = sortOn (view _z . _begin)
            . map (\(Brick p1 p2) -> Brick (min p1 p2) (max p1 p2))

cubesOf :: Brick -> [Cube]
cubesOf (Brick (V3 x1 y1 z1) (V3 x2 y2 z2)) = V3 <$> [x1..x2] <*> [y1..y2] <*> [z1..z2]

fallOne :: (Heights, Brick) -> (Heights, Brick)
fallOne (heights, brick) = (heights', brick') where
    cubes = cubesOf brick
    height = maximum [Map.findWithDefault 0 (V2 x y) heights | (V3 x y _) <- cubes]
    Brick start@(V3 _ _ z) end = brick
    brick' = Brick (start - V3 0 0 (z - height)) (end - V3 0 0 (z - height)) 
    heights' = foldl' go heights (cubesOf brick')
    go hts (V3 x y z') = Map.insert (V2 x y) (z'+1) hts

fall :: [Brick] -> [Brick]
fall = go Map.empty where
    go _ [] = []
    go space (brick:bricks) = brick' : go space' bricks where
        (space', brick') = fallOne (space, brick) 

cubeOwners :: [Brick] -> HashMap (V3 Int) Int
cubeOwners = foldl' go Map.empty . zip [0..] where
    go owners (i, brick) = foldl' 
                            (\owners' cube -> Map.insert cube i owners')
                            owners
                            (cubesOf brick)

precomp :: [Brick] -> ([Brick], Vector [Int], Vector [Int])
precomp bricks = (bricks', support, supported) where
    bricks' = fall (sortBricks bricks)
    owners = cubeOwners bricks'
    supportOf i brick =
        if view _z (_begin brick) == 0
            then [-1]
            else ordNub .catMaybes $ [  j
                                     | cube <- cubesOf brick
                                     , let j = owners Map.!? (cube - V3 0 0 1)
                                     , j /= Just i
                                     ]
    support = V.fromList [supportOf i brick | (i, brick) <- zip [0..] bricks']
    supportedBy i brick =
        ordNub . catMaybes $ [ j 
                             | cube <- cubesOf brick
                             , let j = owners Map.!? (cube + V3 0 0 1)
                             , j /= Just i
                             ]
    supported = V.fromList [supportedBy i brick | (i, brick) <- zip [0..] bricks']

part1 :: ([Brick], Vector [Int], Vector [Int]) -> Int
part1 (bricks, support, supported) = length disintegrated where
    isStable i = length (support V.! i) >= 2
    canBeDisintegrated i = all isStable (supported V.! i)
    disintegrated = [i | (i, _) <- zip [0..] bricks, canBeDisintegrated i]

part2 :: ([Brick], Vector [Int], Vector [Int]) -> Int
part2 (bricks, support, supported) = res where
    nborFunc v disintegrated =
        [ next
        | next <- supported V.! v
        , all (`Set.member` disintegrated) (support V.! next)
        ]
    res = sum [Set.size (reachableFrom' nborFunc i) - 1 | (i, _) <- zip [0..] bricks]

solve :: Text -> IO ()
solve = aoc' parser (Just . precomp) part1 part2