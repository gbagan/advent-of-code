-- https://adventofcode.com/2020/day/7
module AOC2020.Day08 (solve) where
import           AOC.Prelude
import qualified Data.IntSet as Set
import qualified Data.Vector as V
import           AOC (aoc)
import           AOC.Parser (Parser, sepEndBy1, eol, decimal)

data Instr = Noop !Int | Acc !Int | Jmp !Int 

parser :: Parser (Vector Instr)
parser = V.fromList <$> instr `sepEndBy1` eol where
    instr = instr' <* " " <*> signedDecimal
    instr' = Noop <$ "nop" <|> Acc <$ "acc" <|> Jmp <$ "jmp"
    signedDecimal = (id <$ "+" <|> negate <$ "-") <*> decimal

simulate :: Vector Instr -> Either (IntSet, Int) Int
simulate instrs = go 0 0 Set.empty where
    go i acc seen | i >= V.length instrs = Right acc 
                  | i `Set.member` seen = Left (seen, acc)
                  | otherwise = case instrs V.! i of
                    Noop _ -> go (i+1) acc seen' 
                    Jmp j -> go (i+j) acc seen'
                    Acc j -> go (i+1) (acc+j) seen'
            where seen' = Set.insert i seen

part1 :: Vector Instr -> Int
part1 = snd . fromLeft (Set.empty, 0) . simulate

part2 :: Vector Instr -> Maybe Int
part2 instrs = case simulate instrs of
    Right v -> Just v
    Left (seen, _) ->
        listToMaybe . rights $ Set.toList seen <&> \i ->
            case instrs V.! i of
                Noop j -> simulate $ instrs V.// [(i, Jmp j)]
                Jmp j -> simulate $ instrs V.// [(i, Noop j)]
                Acc j -> Left (seen, j)
        
solve :: Text -> IO ()
solve = aoc parser part1 part2