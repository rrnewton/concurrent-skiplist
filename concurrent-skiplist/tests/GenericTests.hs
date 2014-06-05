
-- | Use the generic tests that work for all concurrent hashmaps.

module Main where

import Data.Concurrent.Map.Class as C
import Data.Concurrent.Map.Tests as T
import Data.Concurrent.SkipListMap as SLM

import qualified Test.Framework as TF


main = TF.defaultMain (T.makeConcurrentInsertMapTests (C.new :: IO (SLMap k v)))

