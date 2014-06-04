{-# LANGUAGE TypeFamilies, ConstraintKinds #-}

-- | An abstract interface to concurrent maps.
--
-- This is modeled after `Data.HashTable.Class`

module Data.Concurrent.Map.Class where


import GHC.Prim (Constraint)
--------------------------------------------------------------------------------


-- | A concurrent map that can grow but does not support deletion.
class ConcurrentInsertMap mp where

  -- | Different implementations may place different constraints on
  -- what is required of keys: for example Eq or Hashable.
  type Key mp k :: Constraint
    
  -- | Creates a new concurrent-map.
  new :: IO (mp k v)

  -- | Creates a new map with an expected size.
  newSized :: Int -> IO (mp k v)

  -- | Insert an entry.  This can be called from many different threads concurrently.
  insert :: (Key mp k) => mp k v -> k -> v -> IO ()
  
  -- | Looks up a key-value mapping.  Note that this may race with insert, in which
  -- case either `Nothing` or `Just` could be returned.
  lookup :: (Key mp k) => mp k v -> k -> IO (Maybe v)

  -- foldM
  -- mapM_
  -- computeOverhead


-- | A fully functional concurrent map that allows concurrent deletion as well as
-- insertion.
class ConcurrentInsertMap mp => ConcurrentMap mp where

  -- | Concurrent deletion of an entry in the map
  delete :: (Key mp k) => mp k v -> k -> IO ()
