{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}

-- | Module providing the ability to construct test suites for any concurrent map
-- that satisfies the interface.

module Data.Concurrent.Map.Tests
       ( Phantom(..),
         makeConcurrentInsertMapTests)
       where

import Data.Concurrent.Map.Class

import qualified Test.Framework as TF
import           Test.Framework.Providers.HUnit  (hUnitTestToTests)
import           Test.HUnit as HU
--------------------------------------------------------------------------------

data Phantom (a :: * -> * -> *) = Phantom

-- | Build a series of tests that exercise a concurrent map, without assuming deletion capability
makeConcurrentInsertMapTests :: forall (mp :: * -> * -> *) .
                                ConcurrentInsertMap mp => Phantom mp -> [TF.Test]
makeConcurrentInsertMapTests (_ :: Phantom mp)  =
  []


-- | Make a test suite that exercises a concurrent map INCLUDING concurrent deletion.
--   This includes all the tests from 
-- makeConcurrentMapTests :: ConcurrentMap


