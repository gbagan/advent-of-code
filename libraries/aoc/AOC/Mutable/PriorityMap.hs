-- modified version of https://github.com/bmsherman/priority-map/blob/master/Data/PriorityMap.hs

module AOC.Mutable.PriorityMap (
-- * Priority map type
PriorityMap (..),
-- * Query
AOC.Mutable.PriorityMap.null, lookupValue,
-- * Construction
empty, fromList,
-- * Modification
insert, peek, updateValue, updateMapKey, updatePriority, delete,
-- * Internals
-- | These are not part of the API, and may change from version to version.
Info (..), updateCallback
) where

import AOC.Prelude hiding (empty, fromList)
import AOC.Mutable.Heap (Heap)
import qualified AOC.Mutable.Heap as H
import qualified Data.HashTable.ST.Basic as T
import Control.Monad.ST (ST)
import Data.STRef

-- | The default priority map, which uses an STRef to a Data.Map as its
-- map structure.
-- type PrioMap s = PriorityMap s MapST

data Info km v = Info !Int !km !v deriving Show

-- | A general priority map structure. @s@ is the state thread in the ST monad,
-- @m@ is the map type, @km@ is the keys for the map, @kh@ is the keys for
-- the heap, and @v@ is the value type.
data PriorityMap s km kh v = PriorityMap
  { pmmap  :: T.HashTable s km (STRef s (Info km v)) -- ^ the map
  , pmheap :: Heap s kh (STRef s (Info km v)) -- ^ the heap
  }

-- | Create a new empty priority map.
empty :: ST s (PriorityMap s km kh v)
empty = do
  h <- H.empty
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
    m <- empty
    for_ xs \(k, p, v) -> do
        insert m k p v
    return m

-- | /O(log n)/. Insert a value into the priority map. There should not already
-- be an entry with the given map key. If there is, behavior is undefined,
-- and it is likely that bad things will happen!
insert :: (Hashable k, Ord p) => PriorityMap s k p v -> k -> p -> v -> ST s ()
insert (PriorityMap m h) k p v = do
    info <- newSTRef (Info undefined k v)
    T.insert m k info
    H.insertWith h updateCallback (p, info)

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
            -- todo: updateCallback needed?
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