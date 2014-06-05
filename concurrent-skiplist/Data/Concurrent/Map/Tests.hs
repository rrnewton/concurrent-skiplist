{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}

-- | Module providing the ability to construct test suites for any concurrent map
-- that satisfies the interface.

module Data.Concurrent.Map.Tests
       ( makeConcurrentInsertMapTests
       )
       where

import Data.Concurrent.Map.Class

import qualified Test.Framework as TF
import           Test.Framework.Providers.HUnit  (hUnitTestToTests)
import           Test.HUnit as HU
--------------------------------------------------------------------------------

-- | Build a series of tests that exercise a concurrent map, without assuming deletion capability
--
--   By convention, this takes the "new map function" as input to disambiguate WHICH map we want to test.
makeConcurrentInsertMapTests :: ConcurrentInsertMap mp => IO (mp k v) -> [TF.Test]
makeConcurrentInsertMapTests newFn  =
  []


-- test1 :: IO (String)
-- test1 = do
--   lm <- LM.newLMap
--   LM.NotFound tok <- LM.find lm 1
--   LM.tryInsert tok "Hello"
--   LM.NotFound tok <- LM.find lm 0
--   LM.tryInsert tok " World"
--   LM.Found s1 <- LM.find lm 1
--   LM.Found s0 <- LM.find lm 0
--   return $ s1 ++ s0  


--------------------------------------------------------------------------------

-- | Make a test suite that exercises a concurrent map INCLUDING concurrent deletion.
--   This includes all the tests from 
-- makeConcurrentMapTests :: ConcurrentMap mp => 


