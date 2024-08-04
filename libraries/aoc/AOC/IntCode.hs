module AOC.IntCode where

import           AOC.Prelude
import qualified Data.IntMap.Strict as Map

type Program = IntMap Int
data Effect = Output !Int Effect  | Input (Int -> Effect)  | Halt Machine

data Machine = Machine { program :: Program, offset :: Int, relative :: Int } 

newMachine :: [Int] -> Machine
newMachine pgm = Machine (Map.fromList (zip [0..] pgm)) 0 0

runProgram :: [Int] {- input -} -> [Int] {- program -} -> [Int] {- output -}
runProgram input = go input . runEffect . newMachine where
    go input' = \case
        Halt _ -> []
        Input f -> case input' of
            [] -> error "empty input"
            (i:is) -> go is (f i)
        Output x eff -> x : go input' eff

runEffect :: Machine -> Effect
runEffect = go' where 
    go' machine@(Machine pgm idx relative) =
        let instr = pgm Map.! idx
            opcode = instr `rem` 100
            mode1 = (instr `quot` 100) `rem` 10
            mode2 = (instr `quot` 1000) `rem` 10
            mode3 = (instr `quot` 10000) `rem` 10
        in case opcode of
            1 ->
                let val1 = read relative (idx+1) mode1 pgm
                    val2 = read relative (idx+2) mode2 pgm
                    pgm' = write relative (idx+3) (val1+val2) mode3 pgm
                in go' machine{program = pgm', offset = idx+4}
            2 ->
                let val1 = read relative (idx+1) mode1 pgm
                    val2 = read relative (idx+2) mode2 pgm
                    pgm' = write relative (idx+3) (val1*val2) mode3 pgm
                in go' machine{program = pgm', offset = idx+4}
            3 -> Input (\i ->
                    let pgm' = write relative (idx+1) i mode1 pgm
                    in go' machine{program = pgm', offset = idx+2}
                )
            4 ->
                let o = read relative (idx+1) mode1 pgm in
                Output o $ go' machine{offset = idx+2}
            5 ->
                let val1 = read relative (idx+1) mode1 pgm
                    val2 = read relative (idx+2) mode2 pgm
                in if val1 /= 0
                    then go' machine{offset = val2} 
                    else go' machine{offset = idx+3}
            6 ->
                let val1 = read relative (idx+1) mode1 pgm
                    val2 = read relative (idx+2) mode2 pgm
                in if val1 == 0
                    then go' machine{offset = val2} 
                    else go' machine{offset = idx+3}
            7 ->
                let val1 = read relative (idx+1) mode1 pgm
                    val2 = read relative (idx+2) mode2 pgm
                    pgm' = write relative (idx+3) (fromEnum (val1 < val2)) mode3 pgm
                in go' machine{program=pgm',offset = idx+4}
            8 ->
                let val1 = read relative (idx+1) mode1 pgm
                    val2 = read relative (idx+2) mode2 pgm
                    pgm' = write relative (idx+3) (fromEnum (val1 == val2)) mode3 pgm
                in go' machine{program=pgm',offset = idx+4}
            9 ->
                let val = read relative (idx+1) mode1 pgm
                in go' machine{relative=relative+val, offset = idx+2}
            99 -> Halt machine
            x -> error $ "run: invalid instruction: " <> show x

get :: Int -> Machine -> Int
get idx machine = machine.program Map.!? idx ?: 0

set :: Int -> Int -> Machine -> Machine
set idx val machine = machine{program = Map.insert idx val machine.program}

read :: Int -> Int -> Int -> Program -> Int
read relative idx mode pgm =
    case mode of
        0 -> Map.findWithDefault 0 (pgm Map.! idx) pgm
        1 -> pgm Map.! idx
        _ -> Map.findWithDefault 0 (relative + pgm Map.! idx) pgm

write :: Int -> Int -> Int -> Int -> Program -> Program
write relative idx val mode pgm =
    case mode of
        0 -> Map.insert (pgm Map.! idx) val pgm
        1 -> Map.insert idx val pgm
        _ -> Map.insert (relative + pgm Map.! idx) val pgm