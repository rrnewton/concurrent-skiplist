{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Criterion.Main
import Criterion.Types
import Data.Int
import Data.IORef
import Data.Atomics (atomicModifyIORefCAS)
import Data.Concurrent.LinkedMap as LM
import Data.Concurrent.SkipListMap as SLM
import Data.Concurrent.Map.Bench
import Data.Concurrent.Map.Class
-- import Control.Concurrent

import qualified Data.Map as M

cvt :: PreBench -> Benchmark
cvt PreBench{name,batchRunner} =
    bench name (Benchmarkable batchRunner)

-- Based on vanilla atomicModifyIORef.
-- BAD! This is a SPACE LEAKING VERSION.
newtype IOMap1 k v = IOMap1 (IORef (M.Map k v))
instance ConcurrentInsertMap IOMap1 where
  type Key IOMap1 k = Ord k
  -- FIXME: use safe cast
  new = do x <- newIORef M.empty; return $! IOMap1 x 
  insert (IOMap1 r) k v =
    atomicModifyIORef r (\ m -> (M.insert k v m, ()))
  lookup (IOMap1 r) k = do m <- readIORef r
                           return $! M.lookup k m
  estimateSize (IOMap1 r) = do m <- readIORef r
                               return $! M.size m

-- WHNF strict version:
newtype IOMap2 k v = IOMap2 (IORef (M.Map k v))
instance ConcurrentInsertMap IOMap2 where
  type Key IOMap2 k = Ord k
  -- FIXME: use safe cast
  new = do x <- newIORef M.empty; return $! IOMap2 x 
  insert (IOMap2 r) k v =
    atomicModifyIORef' r (\ m -> (M.insert k v m, ()))
  lookup (IOMap2 r) k = do m <- readIORef r
                           return $! M.lookup k m
  estimateSize (IOMap2 r) = do m <- readIORef r
                               return $! M.size m


-- This version tries the speculative replacement for atomicModifyIORefCAS:
newtype IOMap3 k v = IOMap3 (IORef (M.Map k v))
instance ConcurrentInsertMap IOMap3 where
  type Key IOMap3 k = Ord k
  new = do x <- newIORef M.empty; return $! IOMap3 x 
  insert (IOMap3 r) k v =
    atomicModifyIORefCAS r (\ m -> (M.insert k v m, ()))
  lookup (IOMap3 r) k = do m <- readIORef r
                           return $! M.lookup k m
  estimateSize (IOMap3 r) = do m <- readIORef r
                               return $! M.size m


main :: IO ()
main = do
 suite0 <- mkBenchSuite "SLMap" (Proxy:: Proxy(SLMap Int64 Int64))
 suite1 <- mkBenchSuite "LMap"  (Proxy:: Proxy(LM.LMap Int64 Int64)) 
 suite2 <- mkBenchSuite "PureMap1" (Proxy:: Proxy(IOMap1 Int64 Int64))
 suite3 <- mkBenchSuite "PureMap2" (Proxy:: Proxy(IOMap2 Int64 Int64))
 suite4 <- mkBenchSuite "PureMap3" (Proxy:: Proxy(IOMap3 Int64 Int64))  

 defaultMain $ 
  Prelude.map cvt suite0 ++
  Prelude.map cvt suite1 ++  
  Prelude.map cvt suite2 ++
  Prelude.map cvt suite3 ++
  Prelude.map cvt suite4 ++     
  -- Retain RAW benchmarks to make sure the extra class abstraction
  -- isn't costing us anything:
  [
  bgroup "RAW:linkedMap" [
     bench "new" $ nfIO $ newLMap >> return ()
     , bench "insert" $ nfIO $ do
        lm <- newLMap
        res <- LM.find lm "key"
        case res of
         NotFound tok -> tryInsert tok (42 :: Int) >> return ()
         _ -> return ()
     , bench "fromList10" $ nfIO $ fromList (zip [1..10] [1..10]) >> return ()
     , bench "fillN" $ Benchmarkable $ \num -> do
        lm <- newLMap
        for_ 1 (fromIntegral num) $ \i -> do
          res <- LM.find lm (show i)
          case res of
           NotFound tok -> tryInsert tok i >> return ()
           _ -> return ()
     ]
  , bgroup "RAW:skipListMap" [
     bench "new" $ nfIO $ newSLMap 10 >> return ()
     , bench "insert" $ nfIO $ do
        slm <- newSLMap 10
        putIfAbsent slm "key" $ return (42 :: Int)
        return ()
     , bench "fillN" $ Benchmarkable $ \num -> do
        slm <- newSLMap 10
        for_ 1 (fromIntegral num) $ \i -> do
          putIfAbsent slm (show i) (return i)
          return ()
     ]
  ]

for_ :: Monad m => Int -> Int -> (Int -> m ()) -> m ()
for_ start end _ | start > end = error "start greater than end"
for_ start end fn = loop start
  where loop !i | i > end = return ()
                | otherwise = fn i >> loop (i+1)
{-# INLINE for_ #-}
