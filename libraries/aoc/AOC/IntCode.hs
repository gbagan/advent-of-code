module AOC.IntCode where

import           AOC.Prelude
import qualified Data.IntMap.Strict as Map

type Program = IntMap Int

-- return the output
runProgram :: [Int] -> [Int] -> [Int]
runProgram = runProgram' (const []) (:)

-- return the program
runProgram_ :: [Int] -> [Int] -> [Int]
runProgram_ = runProgram' Map.elems \_ x -> x

runProgram' :: (Program -> a) ->  (Int -> a -> a) ->  [Int] -> [Int] -> a
runProgram' onHalt onOutput initialInput pgmList = go' (Map.fromList (zip [0..] pgmList)) initialInput 0 0 where 
    go' pgm input relative idx =
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
                in go' pgm' input relative (idx+4)
            2 ->
                let val1 = read relative (idx+1) mode1 pgm
                    val2 = read relative (idx+2) mode2 pgm
                    pgm' = write relative (idx+3) (val1*val2) mode3 pgm
                in go' pgm' input relative (idx+4)
            3 -> case input of
                [] -> error "empty input"
                (i:is) ->
                    let pgm' = write relative (idx+1) i mode1 pgm
                    in go' pgm' is relative (idx+2)
            4 ->
                let o = read relative (idx+1) mode1 pgm in
                onOutput o (go' pgm input relative (idx+2))
            5 ->
                let val1 = read relative (idx+1) mode1 pgm
                    val2 = read relative (idx+2) mode2 pgm
                in if val1 /= 0
                    then go' pgm input relative val2 
                    else go' pgm input relative (idx+3)
            6 ->
                let val1 = read relative (idx+1) mode1 pgm
                    val2 = read relative (idx+2) mode2 pgm
                in if val1 == 0
                    then go' pgm input relative val2 
                    else go' pgm input relative (idx+3)
            7 ->
                let val1 = read relative (idx+1) mode1 pgm
                    val2 = read relative (idx+2) mode2 pgm
                    pgm' = write relative (idx+3) (fromEnum (val1 < val2)) mode3 pgm
                in go' pgm' input relative (idx+4)
            8 ->
                let val1 = read relative (idx+1) mode1 pgm
                    val2 = read relative (idx+2) mode2 pgm
                    pgm' = write relative (idx+3) (fromEnum (val1 == val2)) mode3 pgm
                in go' pgm' input relative (idx+4)
            9 ->
                let val = read relative (idx+1) mode1 pgm
                in go' pgm input (relative+val) (idx+2)
            99 -> onHalt pgm
            x -> error $ "run: invalid instruction: " <> show x

read :: Int -> Int -> Int -> Program -> Int
read relative idx mode pgm =
    case mode of
        0 -> pgm Map.! (pgm Map.! idx)
        1 -> pgm Map.! idx
        _ -> pgm Map.! (relative + pgm Map.! idx)

write :: Int -> Int -> Int -> Int -> Program -> Program
write relative idx val mode pgm =
    case mode of
        0 -> Map.insert (pgm Map.! idx) val pgm
        1 -> Map.insert idx val pgm
        _ -> Map.insert (relative + pgm Map.! idx) val pgm