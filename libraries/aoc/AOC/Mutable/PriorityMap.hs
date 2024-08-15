-- modified version of https://github.com/bmsherman/priority-map/blob/master/Data/PriorityMap.hs

module AOC.Mutable.PriorityMap (
-- * Priority map type
PriorityMap (..),
-- * Query
AOC.Mutable.PriorityMap.null, lookupValue,
-- * Construction
new, fromList,
-- * Modification
insert, peek, updateValue, updateMapKey, updatePriority, delete,
-- * Internals
-- | These are not part of the API, and may change from version to version.
Info (..), updateCallback,
AOC.Mutable.PriorityMap.toList
) where

import Prelude
import Control.Arrow (first)
import Control.Monad.ST (ST)
import Data.Foldable (for_)
import Data.Hashable (Hashable)
import qualified Data.HashTable.ST.Basic as T
import Data.STRef
import AOC.Mutable.Heap (Heap)
import qualified AOC.Mutable.Heap as H

data Info p v = Info !Int !p !v deriving Show

-- | A general priority map structure. @s@ is the state thread in the ST monad,
-- @k@ is the key type, @p@ is the priority type and @v@ is the value type.
data PriorityMap s k p v = PriorityMap
  { pmmap  :: T.HashTable s k (STRef s (Info k v))
  , pmheap :: Heap s p (STRef s (Info k v))
  }

-- | Create a new empty priority map.
new :: ST s (PriorityMap s km kh v)
new = do
    h <- H.new
    m <- T.new
    return (PriorityMap m h)

-- | /O(1)/. Returns true iff the priority map is null.
null :: PriorityMap s km kh v -> ST s Bool
null (PriorityMap m _) = (0 ==) <$> T.size m

updateCallback :: H.OnUpdate s (k, STRef s (Info km v))
updateCallback i (_, ref) = modifySTRef' ref (\(Info _ km v) -> Info i km v)

-- | /O(n log n)/. Create a new priority map from a list of map keys, heap
-- scores, and values. Could be improved in the future to be /O(n)/.
fromList :: (Hashable k, Ord p) => [(k, p, v)] -> ST s (PriorityMap s k p v)
fromList xs = do
    m <- new
    for_ xs \(k, p, v) -> do
        insert m k p v
    return m

-- | /O(log n)/. Insert a value into the priority map. If there is already
-- an entry with the given map key, the entry is replaced.
insert :: (Hashable k, Ord p) => PriorityMap s k p v -> k -> p -> v -> ST s ()
insert (PriorityMap t h) k p v = do
    mref <- T.lookup t k
    case mref of
        Nothing -> do
            info <- newSTRef (Info 0 k v)
            T.insert t k info
            H.insert h (p, info)
        Just ref -> do
            Info i _ _ <- readSTRef ref
            writeSTRef ref (Info i k v)
            H.modifyWith h updateCallback i (first (const p))


-- | /O(log n)/. If the priority map is empty, returns 'Nothing'. If the 
-- priority map is not empty, removes the entry with the lowest score from the
-- heap and returns 'Just' that entry.
peek :: (Hashable k, Ord p) => PriorityMap s k p v -> ST s (Maybe (k, p, v))
peek (PriorityMap m h) = do
    res <- H.peekWith h updateCallback
    case res of
        Nothing -> return Nothing
        Just (p, ref) -> do
            (Info _ k v) <- readSTRef ref
            T.delete m k
            return $ Just (k, p, v)

-- | /O(log n)/. Checks whether there is an element in the priority map with
-- the given map key; if there is not, returns 'Nothing', and if there is,
-- returns 'Just' the value associated with that key.
lookupValue :: Hashable k => PriorityMap s k p v -> k -> ST s (Maybe v)
lookupValue (PriorityMap m _) k = do
    r <- T.lookup m k
    case r of
        Nothing -> return Nothing
        Just ref -> do
            Info _ _ v <- readSTRef ref
            return $ Just v

-- | /O(1)/. Modifies the value associated to a given map key. If no entry
-- currently exists for that key, nothing happens.
updateValue :: Hashable k => PriorityMap s k p v -> k -> (v -> ST s v) -> ST s ()
updateValue (PriorityMap m _) k f = do
    mref <- T.lookup m k
    case mref of
        Nothing -> pure ()
        Just ref -> do
            Info i km' v <- readSTRef ref
            v' <- f v
            writeSTRef ref $! Info i km' v'

-- | /O(1)/. Modifies the map key of an entry. If no entry
-- currently exists for that key, nothing happens.
updateMapKey :: Hashable k => PriorityMap s k p v -> k -> k -> ST s ()
updateMapKey (PriorityMap m _) k k' = do
    mref <- T.lookup m k
    case mref of
      Nothing -> pure ()
      Just ref -> do
          T.delete m k
          T.insert m k' ref

-- | /O(log n)/. Delete an entry. If no entry
-- currently exists for that key, nothing happens.
delete :: (Hashable k, Ord p) => PriorityMap s k p v -> k -> ST s ()
delete (PriorityMap t h) k = do
    mref <- T.lookup t k
    case mref of
        Nothing -> pure ()
        Just ref -> do
            T.delete t k
            Info i _ _ <- readSTRef ref
            H.deleteWith h updateCallback i

-- | /O(log n)/. Update the priority for a given entry. If no entry
-- currently exists for that key, behavior is undefined. Currently, an exception
-- should be raised.
updatePriority :: (Hashable k, Ord p) => PriorityMap s k p v -> k -> (p -> p) -> ST s ()
updatePriority (PriorityMap t h) k f = do
    v <- T.lookup t k
    case v of
        Nothing -> pure ()
        Just ref -> do
            Info i _ _ <- readSTRef ref  
            H.modifyWith h updateCallback i (first f)

toList :: PriorityMap s k p v -> ST s [(k, p, v)]
toList (PriorityMap _ h) = traverse go =<< H.toList h where
    go (p, ref) = do
        Info _ k v <- readSTRef ref
        pure (k, p, v)