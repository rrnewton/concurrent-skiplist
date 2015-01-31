{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

-- | An alternative to Data.Concurrent.Map.Class that may be easier to use.

module Data.Concurrent.Map.NoClass
       (ConcurrentInsertMapDict(..))
       where

import Data.Hashable

--------------------------------------------------------------------------------

-- | A record of methods for a concurrent map
data ConcurrentInsertMapDict (mp :: * -> * -> *) =  
  ConcurrentInsertMapDict
  { new :: forall k v . (GoodKey k) =>
           IO (mp k v),

    -- | Creates a new map with an expected size.
    newSized :: forall k v . (GoodKey k) =>
                Int -> IO (mp k v),

    -- | Insert an entry.  This can be called from many different threads concurrently.  
    insert :: forall k v . (GoodKey k) =>
              mp k v -> k -> v -> IO (),
    -- FIXME!  What are the semantics of repeated insertion?

    -- | Looks up a key-value mapping.  Note that this may race with insert, in which
    -- case either `Nothing` or `Just` could be returned.
    lookup :: forall k v . (GoodKey k) =>
              mp k v -> k -> IO (Maybe v)

  }

-- | A simple alias for all the things we might possible ask of keys.
class (Eq k, Ord k, Hashable k) => GoodKey k where
