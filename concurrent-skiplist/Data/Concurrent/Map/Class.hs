

-- | An abstract interface to concurrent maps.
--
-- This is modeled after `Data.HashTable.Class`

module Data.Concurrent.Map.Class where


-- | A concurrent map that can grow but does not support deletion.
class ConcurrentInsertMap mp where

  -- | Constraints on the keys.  Implementation-specific.
--  type 
  
  -- | Creates a new concurrent-map.
  new :: IO (mp k v)

  -- | Creates a new map with an expected size.
  newSized :: Int -> IO (mp k v)

--  insert :: (Eq k, Hashable k) =>

  -- | Insert an entry.  This can be called from many different threads concurrently.
  insert :: mp k v -> k -> v -> IO ()
  -- TODO: constraint
  
  -- | Looks up a key-value mapping.  Note that this may race with insert, in which
  -- case either `Nothing` or `Just` could be returned.
  lookup :: mp k v -> k -> IO (Maybe v)
  -- TODO: constraint

  -- foldM
  -- mapM_
  -- computeOverhead

-- | A fully functional concurrent map that allows concurrent deletion as well as
-- insertion.
class ConcurrentInsertMap mp => ConcurrentMap mp where

  -- | Concurrent deletion of an entry in the map
  delete :: mp k v -> k -> IO ()
  -- TODO: constraint
