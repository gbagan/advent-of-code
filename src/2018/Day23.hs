-- https://adventofcode.com/2018/day/23
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Day23 (solve) where
import           AOC.Prelude hiding (last)
import           AOC (aoc)
import           AOC.List (maximum, minimum, maximumOn)
import           AOC.Parser (Parser, sepEndBy1, eol, signedDecimal, scanf)
import           AOC.V3 (V3(..), origin, manhattan)
import           AOC.Graph (fromEdgePredicate, maximalCliques)

data Bot = Bot { position :: !(V3 Int), radius :: !Int } deriving (Eq, Generic)
instance Hashable Bot

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

part2 :: [Bot] -> Int
part2 bots = snd . minimum . map f $ maximalCliques graph where
    f clique = (-length clique, maximum [distanceToOrigin bot | bot <- clique])
    graph = fromEdgePredicate bots overlaps

solve :: Text -> IO ()
solve = aoc parser part1 part2