-- modified version of https://github.com/bmsherman/priority-map/blob/master/Data/PriorityMap/Heap.hs

module AOC.Mutable.Heap (
-- * The heap type
Heap (..),
-- * Callbacks
OnUpdate, nullCallback,
-- * Construction
new,
-- * Modification
-- | The functions which end in \"With\" allow you to supply an arbitrary
-- callback. The functions which do not use 'nullCallback'.
insertWith, insert, peekWith, peek,
-- * Internal functions
-- | You probably shouldn't call these functions.
bubbleUp, bubbleDown, bubbleUpDown, AOC.Mutable.Heap.lookup,
modifyWith, deleteWith,
AOC.Mutable.Heap.toList
-- * Testing
--Tree (..), inspect,
) where

import Prelude
import qualified AOC.Mutable.DynArray as A
import qualified Data.Vector.Mutable as V
import Control.Monad.ST (ST)
import Data.STRef
import qualified Data.Vector as Vec

data Heap s k v = Heap 
    { payload :: !(A.DynArray s (k, v))
    , size    :: !(STRef s Int)
    }

-- | When this update is called, it means that heap data @kv@ (a score\/value
-- pair) has been moved to a new position (indicated by the Int).
type OnUpdate s kv = Int -> kv -> ST s ()

-- | This callback does nothing.
nullCallback :: OnUpdate s a
nullCallback = const (const (return ()))

-- | Create a new empty heap.
new :: ST s (Heap s k v)
new = do
    arr <- A.empty
    sz <- newSTRef 0
    return (Heap arr sz)

-- | /O(log n)/. Insert a new score\/value pair into the heap (using 
-- 'nullCallback'). 
insert :: Ord k => Heap s k v -> (k, v) -> ST s ()
insert h = insertWith h nullCallback

-- | /O(log n)/. Insert a new score\/value pair into the heap.
insertWith :: Ord k => Heap s k v -> OnUpdate s (k, v) -> (k, v) -> ST s ()
insertWith (Heap arr s) callback kv = do
    sz <- readSTRef s
    let sz' = sz + 1
    A.allocate arr sz'
    writeSTRef s sz'
    vec <- A.getRaw arr
    loc <- bubbleUp vec callback sz kv
    callback loc kv

-- | /O(log n)/. Remove the element with the lowest score from the heap (using
-- 'nullCallback').
peek :: Ord k => Heap s k v -> ST s (Maybe (k, v))
peek h = peekWith h nullCallback

-- | /O(log n)/. Remove the element with the lowest score from the heap. If
-- the heap is empty, return 'Nothing' and make no modifications.
peekWith :: Ord k => Heap s k v -> OnUpdate s (k, v) -> ST s (Maybe (k, v))
peekWith (Heap arr s) callback = do
    sz <- readSTRef s
    if sz == 0 then return Nothing else do
        maxElement <- arr A.! 0
        let sz' = sz - 1
        writeSTRef s sz'
        kv <- arr A.! sz'
        vec <- A.getRaw arr
        loc <- bubbleDown vec callback sz' kv 0
        callback loc kv
        return (Just maxElement)

-- | /O(log n)/. Make a modification to an element located at a particular index
-- in the heap, and adjust the heap accordingly.
modifyWith :: Ord k => Heap s k v -> OnUpdate s (k, v) -> Int -> ((k,v) -> (k,v)) -> ST s ()
modifyWith h@(Heap arr _) callback i f = do
    raw <- A.getRaw arr
    kv <- V.read raw i
    let kv' = f kv
    loc <- bubbleUpDown h callback i kv'
    callback loc kv'

-- | /O(log n)/. Delete an element from a particular index
-- in the heap, and adjust the heap accordingly.
deleteWith :: Ord k => Heap s k v -> OnUpdate s (k, v) -> Int -> ST s ()
deleteWith h@(Heap arr s) callback i = do
    sz <- readSTRef s
    kv <- arr A.! (sz - 1)
    writeSTRef s (sz - 1)
    loc <- bubbleUpDown h callback i kv
    callback loc kv

-- | /O(1)/. Lookup the score\/value pair at a particular index in the heap.
lookup :: Heap s k v -> Int -> ST s (k, v)
lookup (Heap arr _) i = arr A.! i

--
-- Bubble up and down
--

-- | Bubble up or down, depending on what is appropriate.
bubbleUpDown :: Ord k => Heap s k v -> OnUpdate s (k, v) -> Int -> (k, v) -> ST s Int
bubbleUpDown (Heap arr s) callback i kv = do
    raw <- A.getRaw arr
    loc <- bubbleUp raw callback i kv
    if i == loc
    then do
        sz <- readSTRef s
        bubbleDown raw callback sz kv i
    else return loc

-- | Returns the final location of the thing that is bubbled up.
bubbleUp :: Ord k => V.MVector s (k, v) -> OnUpdate s (k, v) -> Int -> (k, v) -> ST s Int
bubbleUp vec callback i kv = if i == 0 then stop else do
    parent <- V.read vec (par i)
    if fst kv <= fst parent
        then stop
        else do
            V.write vec i parent
            callback i parent
            bubbleUp vec callback (par i) kv
    where
    stop = V.write vec i kv >> return i

-- | Returns the final location of the thing that is bubbled down.
bubbleDown :: Ord k => V.MVector s (k, v) -> OnUpdate s (k, v) -> Int -> (k, v) -> Int -> ST s Int
bubbleDown vec callback sz kv@(k, _) i = do
    children <- mapM get $ filter (< sz) [chl i, chr i]
    let bigchild = case children of
            [] -> Nothing
            [c] -> Just c
            (c1:c2:_) | prio c1 >= prio c2 -> Just c1
                      | otherwise          -> Just c2 
    case bigchild of
        Just (i', child) | fst child > k -> do
            V.write vec i child
            callback i child
            bubbleDown vec callback sz kv i'
        _ -> V.write vec i kv >> return i
    where 
    get idx = (,) idx <$> V.read vec idx
    prio = fst . snd


-- | The parent index of a given index.
par :: Int -> Int
par n = (n-1) `div` 2
{-# INLINE par #-}

-- | The left child index of a given index.
chl :: Int -> Int
chl n = 2 * n + 1
{-# INLINE chl #-}

-- | The right child index of a given index.
chr :: Int -> Int
chr n = 2 * n + 2
{-# INLINE chr #-}


toList :: Heap s k v -> ST s [(k, v)]
toList (Heap arr s) = do
    raw <- A.getRaw arr
    sz <- readSTRef s
    let raw' = V.slice 0 sz raw
    Vec.toList <$> Vec.freeze raw'

--
-- Functions for testing
--
{-
data Tree a = Node a (Tree a) (Tree a) | Leaf

instance Show a => Show (Tree a) where
    show = unlines . printTree 0 where
        printTree _ Leaf = []
        printTree n (Node x l r) = printTree (n+2) l ++ ((replicate n ' ' ++ show x) : printTree (n+2) r)

-- | Return a tree that represents the current heap.
inspect :: Heap s k v -> ST s (Tree (k, v))
inspect (Heap arr s) = do
    sz <- readSTRef s
    inspectFromTo arr 1 sz

inspectFromTo :: A.DynArray s a -> Int -> Int -> ST s (Tree a)
inspectFromTo arr start sz = if start <= sz
    then do
        me <- arr A.! start
        left <- inspectFromTo arr (chl start) sz
        right <- inspectFromTo arr (chr start) sz
        return (Node me left right)
    else return Leaf
-}