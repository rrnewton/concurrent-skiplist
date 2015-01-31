{-# LANGUAGE RecordWildCards, BangPatterns #-}
{-# LANGUAGE ConstraintKinds, ScopedTypeVariables #-}

-- | A module providing a common benchmark suite for mutable concurrent maps.

module Data.Concurrent.Map.Bench
       (PreBench(..), mkBenchSuite, Proxy(..))
       where

import Control.Exception
-- import Control.DeepSeq
import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async (wait, withAsyncOn)
import qualified Data.Concurrent.Map.Class as C
import Data.Concurrent.LinkedMap as LM
import Data.Concurrent.SkipListMap as SLM
import Data.Int
import System.Mem (performGC)
--------------------------------------------------------------------------------

data Proxy a = Proxy

-- | To avoid a dependence on Criterion, we deal in this simple
-- datatype which can easily be turned into a benchmark.
data PreBench = PreBench { name :: String
                         , batchRunner :: Int64 -> IO () }

-- Hard coding these for now:
type Key = Int64
type Val = Int64

mkBenchSuite :: forall mp . (C.ConcurrentInsertMap mp, C.Key mp Key) =>
                String -> Proxy (mp Key Val) -> IO [PreBench]
mkBenchSuite name Proxy = do
  let fillN :: Int64 -> IO (mp Key Val)
      fillN num = do
        mp <- C.new
        blit mp num 
        return mp
      blit :: (mp Key Val) -> Int64 -> IO ()
      blit mp num = for_ 1 num $ \i -> C.insert mp i i

  let sizes = [ 10^e | e <- [0,1,2,3,4::Int]]
  numCap <- getNumCapabilities
  -- rep performGC 5 -- For pushing data to oldest generation.
  putStrLn "Done preallocating."
  return $
    [ PreBench (name++"/new") (rep (C.new :: IO (mp Key Val))) ] ++
    [ PreBench (name++"/insert"++show n) (rep (fillN n))
    | n <- sizes ] ++ 

    -- Parallel benchmarks:
    [ PreBench (name++"/parBarrier-insert"++show elems)
      -- Here we have the sequential loop on the outside, barriers for
      -- each criterion iteration:
      (rep (do mp <- C.new
               forkJoin numCap (\_ -> blit mp elems)))
    | elems <- sizes ]

    -- Here we instead elide the barriers and pre-allocate the
    -- structures we will operate on.  We thus allow different
    -- criterion iterations to actually overlap in real time.  This
    -- models performance inside an application with abundant
    -- parallelism.
    


-- | Prebuild N empty copies of a structure so we can communicate
--   their locations once, and blast them all in parallel.
makeNfillN = do
  undefined

-- | Run N copies of an IO action in parallel.  Pass in a number from
-- 0..N-1, letting the worker know which it is.
forkJoin :: Int -> (Int -> IO ()) -> IO ()
forkJoin num act = loop num []
 where
  -- This strategy makes things exception safe:
  loop 0 ls = mapM_ wait ls
  loop n ls = withAsyncOn n (act (n-1)) $ \ asnc -> 
               loop (n-1) (asnc:ls)


{- 
main :: IO ()
main = defaultMain [
  bgroup "linkedMap" [
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
  , bgroup "skipListMap" [
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
-} 


rep :: Monad m => m a -> Int64 -> m ()
rep m n = for_ 1 n (\_ -> m)

-- | Inclusive range.
for_ :: Monad m => Int64 -> Int64 -> (Int64 -> m a) -> m ()
for_ start end _ | start > end = error "start greater than end"
for_ start end fn = loop start
  where loop !i | i > end = return ()
                | otherwise = fn i >> loop (i+1)
{-# INLINE for_ #-}
