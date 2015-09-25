
-- | Use the generic tests that work for all concurrent hashmaps.

module Main where

import Data.Concurrent.Map.Class
import Data.Concurrent.Map.Tests as T
import Data.Concurrent.SkipListMap as SLM

import qualified Test.Tasty as TST


main = TST.defaultMain (T.makeConcurrentInsertMapTests (Phantom :: Phantom SLMap))
