module AOC.IntCode where

import           AOC.Prelude
import           Control.Monad.ST (ST, runST)
import qualified Data.Vector.Unboxed as V
import           Data.Vector.Unboxed.Mutable (MVector, read, write)

readData :: MVector s Int -> Int -> Int -> ST s Int
readData pgm idx mode = do
    val <- read pgm idx
    if mode /= 0
        then pure val
        else read pgm val

writeData :: MVector s Int -> Int -> Int -> Int -> ST s ()
writeData pgm idx val mode = do
    if mode /= 0 
        then write pgm idx val
        else do
            idx' <- read pgm idx
            write pgm idx' val

runProgram :: [Int] -> V.Vector Int -> (V.Vector Int, [Int])
runProgram input v = runST do
    pgm <- V.thaw v
    go pgm input [] 0

go :: MVector s Int -> [Int] -> [Int] -> Int -> ST s (V.Vector Int, [Int])
go pgm input output idx = do
        instr <- read pgm idx
        let opcode = instr `rem` 100
        let mode1 = (instr `quot` 100) `rem` 10
        let mode2 = (instr `quot` 1000) `rem` 10
        let mode3 = (instr `quot` 10000) `rem` 10
        case opcode of
            1 -> do
                val1 <- readData pgm (idx+1) mode1
                val2 <- readData pgm (idx+2) mode2
                writeData pgm (idx+3) (val1+val2) mode3
                go pgm input output (idx+4)
            2 -> do
                val1 <- readData pgm (idx+1) mode1
                val2 <- readData pgm (idx+2) mode2
                writeData pgm (idx+3) (val1*val2) mode3
                go pgm input output (idx+4)
            3 -> do
                case input of
                    [] -> error "no input"
                    (i:is) -> do
                        writeData pgm (idx+1) i mode1
                        go pgm is output (idx+2)
            4 -> do
                o <- readData pgm (idx+1) mode1
                go pgm input (o:output) (idx+2)
            5 -> do
                val1 <- readData pgm (idx+1) mode1
                val2 <- readData pgm (idx+2) mode2
                if val1 /= 0
                    then go pgm input output val2
                    else go pgm input output (idx+3)
            6 -> do
                val1 <- readData pgm (idx+1) mode1
                val2 <- readData pgm (idx+2) mode2
                if val1 == 0
                    then go pgm input output val2
                    else go pgm input output (idx+3)
            7 -> do
                val1 <- readData pgm (idx+1) mode1
                val2 <- readData pgm  (idx+2) mode2
                writeData pgm (idx+3) (fromEnum (val1 < val2)) mode3
                go pgm input output (idx+4)
            8 -> do
                val1 <- readData pgm (idx+1) mode1
                val2 <- readData pgm (idx+2) mode2
                writeData pgm (idx+3) (fromEnum (val1 == val2)) mode3
                go pgm input output (idx+4)
            99 -> do
                pgm' <- V.freeze pgm
                pure (pgm', output)
            x -> error $ "run: invalid instruction: " <> show x