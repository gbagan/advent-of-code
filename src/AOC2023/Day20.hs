-- https://adventofcode.com/2023/day/20
module AOC2023.Day20 (solve) where
import           AOC.Prelude hiding (Type, head, round, state)
import           Data.List (head)
import           AOC (aoc)
import qualified Data.HashMap.Strict as Map
import qualified Data.Sequence as Seq 
import           Data.Sequence (Seq(..), (><))
import           Lens.Micro (ix)
import           Lens.Micro.Mtl ((.=), (+=))
import           Lens.Micro.TH (makeLenses)
import           Lens.Micro.Platform ()
import           AOC.Parser (Parser, sepBy1, sepEndBy1, some, lowerChar, eol)


data Type = FlipFlop | Conjunction | Broadcaster deriving (Show)
data Module = Module !Type [String] deriving (Show)
type Network = HashMap String Module

parser :: Parser Network
parser = insertRx . Map.fromList <$> module_ `sepEndBy1` eol where
    module_ = do
        t <- type_
        n <- name <* " -> "
        ns <- name `sepBy1` ", "
        pure (n, Module t ns)
    name = some lowerChar
    type_ = FlipFlop <$ "%" <|> Conjunction <$ "&" <|> pure Broadcaster
    insertRx = Map.insert "rx" (Module Broadcaster [])

data NState = NState 
    { _ffState :: !(HashMap String Bool)  -- the state of flip flap mdoules
    , _from :: !(HashMap String (HashMap String Bool)) -- last signal sent by predecessor
    , _nbLow :: !Int
    , _nbHigh :: !Int
    , _seen :: !(HashMap String Bool)
    }

makeLenses ''NState

sendSignal :: Network -> Seq (String, String, Bool) -> State NState ()
sendSignal network = \case
    Seq.Empty -> pure ()
    ((name, srcName, signal) :<| queue') -> do
        if signal then
            nbHigh += 1
        else do
            nbLow += 1
            seen . ix name .= True
        let Module type_ dests = network Map.! name
        case type_ of
            Broadcaster ->
                sendSignal network $ queue' >< Seq.fromList (map (,name, False) dests)
            FlipFlop ->
                if signal then -- low signal 
                    sendSignal network queue'
                else do 
                    nstate <- get
                    let state = _ffState nstate Map.! name 
                    ffState . ix name .= not state
                    sendSignal network $ queue' >< Seq.fromList (map (,name, not state) dests)
            Conjunction -> do
                from . ix name . ix srcName .= signal
                nstate <- get
                let signal' = any not $ Map.elems (_from nstate Map.! name)
                sendSignal network $ queue' >< Seq.fromList (map (,name, signal') dests)

round :: Network -> State NState ()
round network = do
    seen .= Map.map (const False) network
    sendSignal network $ Seq.singleton ("broadcaster", "$dummy", False)

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
    nstate = initNState network
    finalState = flip execState nstate do
        forM_ [(1::Int)..1000] \_ -> round network

part2 :: Network -> Integer
part2 network = foldl' lcm 1 cycles where
    nstate = initNState network
    predRx = head . Map.keys $ _from nstate Map.! "rx"
    predPredRx = Map.keys $ _from nstate Map.! predRx
    nstates = iterate' (execState (round network)) nstate
    cycles = map extractCycle predPredRx
    extractCycle name = head [ idx 
                             | (idx, True) <- zip [0..] 
                                . map ((Map.! name) . _seen) 
                                 $ nstates
                             ]

solve :: Text -> IO ()
solve = aoc parser part1 part2