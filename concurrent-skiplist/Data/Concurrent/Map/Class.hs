{-# LANGUAGE TypeFamilies, ConstraintKinds #-}

-- | An abstract interface to concurrent maps.
--
-- This is modeled after `Data.HashTable.Class`

module Data.Concurrent.Map.Class
       (ConcurrentInsertMap(..), ConcurrentMap(..))
       where

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
  newSized _ = new

  -- | Insert an entry.  This can be called from many different threads concurrently.
  insert :: (Key mp k) => mp k v -> k -> v -> IO ()
  -- FIXME!  What are the semantics of repeated insertion?

  -- | Looks up a key-value mapping.  Note that this may race with insert, in which
  -- case either `Nothing` or `Just` could be returned.
  lookup :: (Key mp k) => mp k v -> k -> IO (Maybe v)

  -- foldM
  -- mapM_
  -- computeOverhead

  -- TODO: Some form of approximate (lower bound) on size would be useful,
  -- many data structures could provide this, and others could just say "zero".

  -- TODO: Should there be a notion of "freezing" so as to enable non-threadsafe
  -- operations, like computing the exact size?
  -- (This concept is present in LVish, but might make sense at this level as well.)


-- | A fully functional concurrent map that allows concurrent deletion as well as
-- insertion.
class ConcurrentInsertMap mp => ConcurrentMap mp where

  -- | Concurrent deletion of an entry in the map
  delete :: (Key mp k) => mp k v -> k -> IO ()
