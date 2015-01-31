{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ConstraintKinds #-}

-- | Module providing the ability to construct test suites for any concurrent map
-- that satisfies the interface.

module Data.Concurrent.Map.Tests
       ( Phantom(..),
         makeConcurrentInsertMapTests)
       where

import Data.Concurrent.Map.Class as C

import qualified Test.Framework as TF
import           Test.Framework.Providers.HUnit  (testCase, hUnitTestToTests)
import           Test.HUnit as HU
import Prelude hiding (lookup)
--------------------------------------------------------------------------------

data Phantom (a :: * -> * -> *) = Phantom

-- | Build a series of tests that exercise a concurrent map, without assuming deletion capability
makeConcurrentInsertMapTests :: forall (mp :: * -> * -> *) .
                                (ConcurrentInsertMap mp, Key mp Int, Key mp String) =>
                                Phantom mp -> [TF.Test]
makeConcurrentInsertMapTests (p :: Phantom mp)  =
  [ testCase "test1" (test1 p)
  ]


test1 :: forall mp . (ConcurrentInsertMap mp, Key mp Int) =>
         Phantom mp -> HU.Assertion
test1 (_ :: Phantom mp) = do
  (m :: mp Int String) <- C.new
  Nothing <- lookup m 1
  insert m 1 "Hello"
  Nothing <- lookup m 0
  insert m 0 " World"
  Just s1 <- lookup m 1
  Just s0 <- lookup m 0
  HU.assertEqual "Read what we wrote" "Hello World" (s1++s0)

--------------------------------------------------------------------------------

-- | Make a test suite that exercises a concurrent map INCLUDING concurrent deletion.
--   This includes all the tests from 
-- makeConcurrentMapTests :: ConcurrentMap


