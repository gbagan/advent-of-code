-- https://adventofcode.com/2023/day/20
module Day20 (solve) where
import           AOC.Prelude hiding (Type, round, state)
import           AOC (aoc)
import qualified Data.HashMap.Strict as Map
import qualified Data.Sequence as Seq 
import           Data.Sequence (Seq(..), (><))
import qualified Data.Text as Text
import           Lens.Micro (ix)
import           Lens.Micro.Mtl ((.=), (+=))
import           Lens.Micro.TH (makeLenses)
import           Lens.Micro.Platform ()
import           AOC.List (headMaybe)
import           AOC.Parser (Parser, sepBy1, sepEndBy1, some, lowerChar, eol)
import           AOC.Util (timesM_)

data Type = FlipFlop | Conjunction | Broadcaster
data Module = Module !Type [Text]
type Network = HashMap Text Module

data NState = NState 
    { _ffState :: !(HashMap Text Bool)  -- the state of flip flap mdoules
    , _from :: !(HashMap Text (HashMap Text Bool)) -- last signal sent by predecessor
    , _nbLow :: !Int
    , _nbHigh :: !Int
    , _seen :: !(HashMap Text Bool)
    }

makeLenses ''NState

parser :: Parser Network
parser = insertRx . Map.fromList <$> module_ `sepEndBy1` eol where
    module_ = do
        t <- type_
        n <- label <* " -> "
        ns <- label `sepBy1` ", "
        pure (n, Module t ns)
    label = Text.pack <$> some lowerChar
    type_ = FlipFlop <$ "%" <|> Conjunction <$ "&" <|> pure Broadcaster
    insertRx = Map.insert "rx" (Module Broadcaster [])

sendSignal :: Network -> Seq (Text, Text, Bool) -> State NState ()
sendSignal network = \case
    Seq.Empty -> pure ()
    ((name, srcName, pulse) :<| queue') -> do
        if pulse then
            nbHigh += 1
        else do
            nbLow += 1
            seen . ix name .= True
        let Module type_ dests = network Map.! name
        case type_ of
            Broadcaster ->
                sendSignal network $ queue' >< Seq.fromList (map (,name, False) dests)
            FlipFlop ->
                if pulse then 
                    sendSignal network queue'
                else do 
                    nstate <- get
                    let state = _ffState nstate Map.! name 
                    ffState . ix name .= not state
                    sendSignal network $ queue' >< Seq.fromList (map (,name, not state) dests)
            Conjunction -> do
                from . ix name . ix srcName .= pulse
                nstate <- get
                let signal' = any not $ Map.elems (_from nstate Map.! name)
                sendSignal network $ queue' >< Seq.fromList (map (,name, signal') dests)

pushButton :: Network -> State NState ()
pushButton network = do
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
        timesM_ 1000 (pushButton network)


extractCycle :: [NState] -> Text -> Maybe Integer
extractCycle states name = headMaybe [ idx 
                                     | (idx, True) <- zip [0..] 
                                     $ map ((Map.! name) . _seen) states
                                     ]

part2 :: Network -> Maybe Integer
part2 network = do
    let nstate = initNState network
    predRx <- headMaybe . Map.keys $ _from nstate Map.! "rx"
    let predPredRx = Map.keys $ _from nstate Map.! predRx
    let nstates = iterate' (execState (pushButton network)) nstate
    cycles <- traverse (extractCycle nstates) predPredRx
    pure $ foldl' lcm 1 cycles

solve :: Text -> IO ()
solve = aoc parser part1 part2