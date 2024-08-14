-- https://adventofcode.com/2018/day/23
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Day23 (solve) where
import           AOC.Prelude hiding (last)
import           AOC (aoc)
import           AOC.List (maximum, minimum, maximumOn)
import           AOC.Parser (Parser, sepEndBy1, eol, signedDecimal, scanf)
import           AOC.V3 (V3(..), origin, manhattan)
import           AOC.Graph (Graph, maximalCliques)
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set

data Bot = Bot { position :: !(V3 Int), radius :: !Int }

parser :: Parser [Bot]
parser = bot `sepEndBy1` eol where
    bot = do
        (x, y, z, r) <- [scanf|pos=<{d},{d},{d}>, r={d}|]
        pure $ Bot (V3 x y z) r
    d = signedDecimal

part1 :: [Bot] -> Int
part1 bots = length [() | Bot pos' _ <- bots, manhattan pos pos' <= r]  where
    Bot pos r = maximumOn radius bots

overlaps :: Bot -> Bot -> Bool
overlaps (Bot pos1 radius1) (Bot pos2 radius2) = manhattan pos1 pos2 <= radius1 + radius2

distanceToOrigin :: Bot -> Int
distanceToOrigin (Bot pos radius) = manhattan origin pos - radius

mkGraph :: [Bot] -> Graph Int
mkGraph bots = Map.fromList [ (i, Set.fromList [ j 
                                               | (j, bot') <- zip [0..] bots
                                               , i /= j && overlaps bot bot'
                                               ]
                              )
                            | (i, bot) <- zip [0..] bots
                            ]

part2 :: [Bot] -> Int
part2 bots = snd . minimum . map f $ maximalCliques graph where
    f indices = (-length indices, maximum [distanceToOrigin (vbots V.! i) | i <- indices])
    vbots = V.fromList bots
    graph = mkGraph bots

solve :: Text -> IO ()
solve = aoc parser part1 part2