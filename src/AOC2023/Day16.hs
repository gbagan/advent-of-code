-- https://adventofcode.com/2023/day/16
module AOC2023.Day16 (solve) where
import           AOC.Prelude hiding (toList)
import           Data.List (maximum)
import qualified Data.HashSet as Set
import           AOC (aoc)
import           AOC.V2 (V2(..), toIx2)
import           AOC.Parser (Parser, sepEndBy1, eol, choice, some)
import           AOC.Search (dfs)
import           Data.Massiv.Array (Matrix, (!), (!?), fromLists', size, B, Comp(Seq), Sz(Sz2))

data Tile = Empty | Horizontal | Vertical | Slash | Antislash
type Coord = V2 Int
type Direction = V2 Int
type Input = Matrix B Tile

parser :: Parser Input  
parser = fromLists' @B Seq <$> some tile `sepEndBy1` eol where
    tile = choice [ Empty <$ "."
                  , Horizontal <$ "-"
                  , Vertical <$ "|"
                  , Slash <$"/"
                  , Antislash <$ "\\"
                  ]

nextDirections :: Direction -> Tile -> [Direction]
nextDirections (V2 dr dc) = \case
    Slash -> [V2 (-dc) (-dr)]
    Antislash -> [V2 dc dr]
    Horizontal | dr /= 0 -> [V2 0 (-1), V2 0 1]
    Vertical | dc /= 0 -> [V2 (-1) 0, V2 1 0]
    _ -> [V2 dr dc]

energized :: (Coord, Direction) -> Input -> Int
energized start grid = Set.size . Set.fromList . map fst $ dfs nbors start where
    nbors (pos, dir) = [ (nextPos, nextDir)
                       | nextDir <- nextDirections dir (grid ! toIx2 pos)  
                       , let nextPos = pos + nextDir
                       , isJust (grid !? toIx2 nextPos)
                       ]

part1 :: Input -> Int
part1 = energized (V2 0 0, V2 0 1)

part2 :: Input -> Int
part2 grid = maximum [energized start grid | start <- starts] where
    Sz2 h w = size grid
    starts = concat $
                [[(V2 r 0, V2 0 1), (V2 r (w-1), V2 0 (-1))] | r <- [0 .. h-1]]
             ++ [[(V2 0 c, V2 1 0), (V2 (h-1) c, V2 (-1) 0)] | c <- [0 .. w-1]]

solve :: Text -> IO ()
solve = aoc parser part1 part2