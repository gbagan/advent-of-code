-- https://adventofcode.com/2023/day/20
module AOC2023.Day20 (solve) where
import           AOC.Prelude hiding (Type, round, state)
import           Data.List ((!!))
import           AOC (aoc)
import qualified Data.HashMap.Strict as Map
import           AOC.Parser (Parser, sepBy1, sepEndBy1, some, lowerChar, eol)

data Type = FlipFlop | Conjunction | Broadcaster
data Instr = Instr !Type ![String]
type Network = HashMap String Instr

parser :: Parser Network
parser = Map.fromList <$> instr `sepEndBy1` eol where
    instr = do
        t <- type_
        n <- name <* " -> "
        ns <- name `sepBy1` ", "
        pure (n, Instr t ns)
    name = some lowerChar
    type_ = FlipFlop <$ "%" <|> Conjunction <$ "&" <|> Broadcaster <$ ""

type NState = 
    ( HashMap String Bool
    , HashMap String (HashMap String Bool)
    , Int
    , Int
    )

round :: Network -> NState -> NState
round network acc = sendSignal acc ("broadcaster", "$dummy", False) where
    sendSignal (states, from, nbLow, nbHigh) (name, srcName, isHigh) = (states'', from'', nbLow'', nbHigh'') where
        nbLow' = nbLow + fromEnum (not isHigh)
        nbHigh' = nbHigh + fromEnum isHigh
        Instr type_ dest = network Map.! name
        state = states Map.! name
        (state', sendHigh, send, from') = case (type_, isHigh) of
            (Broadcaster, _) -> (state, False, True, from)
            (FlipFlop, True) -> (state, False, False, from)
            (FlipFlop, False) -> (not state, not state, True, from)
            (Conjunction, _) -> 
                let from3 = Map.adjust (Map.insert srcName isHigh) name from
                    sendHigh' = any not $ Map.elems (from3 Map.! name)
                in
                    (state, sendHigh', True, from3)
        states' = Map.insert name state' states
        (states'', from'', nbLow'', nbHigh'') =
            if send then
                foldl' sendSignal (states', from', nbLow', nbHigh') (map (,name,sendHigh) dest) 
            else
                (states', from', nbLow', nbHigh')

part1 :: Network -> Int
part1 network = nbLow * nbHigh  where
    network' = Map.insert "rx" (Instr Broadcaster []) network
    initStates = Map.map (const False) network'
    emptyFrom = Map.map (const Map.empty) network'
    edgeList = concat . Map.elems $ Map.mapWithKey (\u (Instr _ vs) -> map (u,) vs) network' 
    !initFrom = foldl' (\fr (u, v) -> Map.adjust (Map.insert u False) v fr) emptyFrom edgeList
    (_, _, nbLow, nbHigh) = traceShow (emptyFrom Map.! "rx") $ iterate' (round network') (initStates, initFrom, 0, 0) !! 1000

part2 :: Network -> Int
part2 _ = 1

solve :: Text -> IO ()
solve = aoc parser part1 part2