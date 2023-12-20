-- https://adventofcode.com/2023/day/20
module AOC2023.Day20 (solve) where
import           AOC.Prelude hiding (Type, head, round, state)
import           Data.List (head)
import           AOC (aoc)
import qualified Data.HashMap.Strict as Map
import           Lens.Micro (ix)
import           Lens.Micro.Mtl ((.=), (+=))
import           Lens.Micro.TH (makeLenses)
import           Lens.Micro.Platform ()
import           AOC.Parser (Parser, sepBy1, sepEndBy1, some, lowerChar, eol)

data Type = FlipFlop | Conjunction | Broadcaster deriving (Show)
data Module = Module !Type [String] deriving (Show)
type Network = HashMap String Module

parser :: Parser Network
parser = Map.fromList <$> module_ `sepEndBy1` eol where
    module_ = do
        t <- type_
        n <- name <* " -> "
        ns <- name `sepBy1` ", "
        pure (n, Module t ns)
    name = some lowerChar
    type_ = FlipFlop <$ "%" <|> Conjunction <$ "&" <|> pure Broadcaster

data NState = NState 
    { _ffState :: !(HashMap String Bool)  -- the state of flip flap mdoules
    , _from :: !(HashMap String (HashMap String Bool)) -- last signal sent by predecessor
    , _nbLow :: !Int
    , _nbHigh :: !Int
    , _seen :: !(HashMap String Bool)
    }

makeLenses ''NState

round :: Network -> State NState ()
round network = do
        seen .= Map.map (const False) network
        sendSignal "broadcaster" "$dummy" False
    where
    sendSignal name srcName signal = do
        if signal then  
            nbHigh += 1
        else do
            nbLow += 1
            seen . ix name .= True
        let Module type_ dests = network Map.! name
        case type_ of
            Broadcaster ->
                forM_ dests \dest ->
                    sendSignal dest name False 
            FlipFlop ->
                unless signal do -- low signal 
                    nstate <- get
                    let state = _ffState nstate Map.! name 
                    ffState . ix name .= not state
                    forM_ dests \dest ->
                        sendSignal dest name (not state)
            Conjunction -> do
                from . ix name . ix srcName .= signal
                nstate <- get
                let signal' = any not $ Map.elems (_from nstate Map.! name)
                forM_ dests \dest ->
                    sendSignal dest name signal'

initNState :: Network -> NState
initNState network = NState initFfState initFrom 0 0 initSeen where
    initFfState = Map.map (const False) network
    emptyFrom = Map.map (const Map.empty) network
    edgeList = concat . Map.elems $ Map.mapWithKey go network 
    go u (Module _ vs) = map (u,) vs
    initFrom = foldl' go' emptyFrom edgeList
    go' from_ (u, v) = Map.adjust (Map.insert u False) v from_
    initSeen =  Map.map (const False) network

part1 :: Network -> Int
part1 network = _nbLow finalState * _nbHigh finalState where
    network' = Map.insert "rx" (Module Broadcaster []) network
    nstate = initNState network'
    finalState = flip execState nstate do
        forM_ [(1::Int)..1000] \_ -> round network'

part2 :: Network -> Integer
part2 network = foldl' lcm 1 cycles where
    network' = Map.insert "rx" (Module Broadcaster []) network
    nstate = initNState network'
    predRx = head . Map.keys $ _from nstate Map.! "rx"
    predPredRx = Map.keys $ _from nstate Map.! predRx
    nstates = iterate' (execState (round network')) nstate
    cycles = predPredRx <&> \name ->
        findCycle [idx |  (idx, True) <- zip [0..] . map ((Map.! name) . _seen) $ nstates]
    findCycle (x:y:_) = y - x
    findCycle _ = error "findCycle: cannot happen"

solve :: Text -> IO ()
solve = aoc parser part1 part2