-- modified version of https://github.com/bmsherman/priority-map/blob/master/Data/PriorityMap/DynArray.hs

module AOC.Mutable.DynArray where
import           Prelude
import           Control.Monad.ST (ST)
import qualified Data.Vector.Mutable as V
import Data.STRef

-- data DynArrayState s e = DState  
-- | A dynamically resizeable array in the ST monad.
data DynArray s e = DynArray
    {   -- | A mutable reference to the current array that is in use.
    payload :: STRef s (V.MVector s e),
        -- | The growth strategy of the array. When the array is grown, this
        -- function takes the old array size and generates the new array size.
        -- This function should be monotonically increasing. 
    growth :: Int -> Int
    }

-- newtype DynArray = DynArray (STRef s (DynArrayState s e))

-- | Create a new, empty array, with a current capacity of 16, which doubles
-- in size whenever it exceeds capacity.
empty :: ST s (DynArray s e)
empty = emptyWith 16 id

-- | Create a new, empty array with a particular size and growth strategy.
emptyWith :: Int -> (Int -> Int) -> ST s (DynArray s e)
emptyWith s gr = do
    arr <- V.unsafeNew s
    ref <- newSTRef arr
    return $ DynArray ref gr

-- | Read from the array at a given index. If the index has not yet been written
-- to, behavior is undefined.
(!) :: DynArray s e -> Int -> ST s e
arr ! i = do
    a <- readSTRef (payload arr)
    V.read a i

getRaw :: DynArray s e -> ST s (V.MVector s e)
getRaw arr = readSTRef (payload arr)

allocate :: DynArray s e -> Int -> ST s ()
allocate arr sz = do
    a <- readSTRef (payload arr)
    let sz' = V.length a
    if sz' >= sz
        then pure ()
        else do
            a' <- V.grow a (max sz (2 * sz))
            writeSTRef (payload arr) a'