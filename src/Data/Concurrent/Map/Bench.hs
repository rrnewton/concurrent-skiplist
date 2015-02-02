{-# LANGUAGE RecordWildCards, BangPatterns #-}
{-# LANGUAGE ConstraintKinds, ScopedTypeVariables #-}

-- | A module providing a common benchmark suite for mutable concurrent maps.

module Data.Concurrent.Map.Bench
       (PreBench(..), mkBenchSuite, Proxy(..),
        forkJoin, rep, for_)
       where

import Control.Exception
import Control.Monad
-- import Control.DeepSeq
import Control.Concurrent (getNumCapabilities, forkOn, forkIO, myThreadId)
import Control.Concurrent.MVar
import Control.Concurrent.Async (wait, withAsyncOn)
import qualified Data.Concurrent.Map.Class as C
import Data.Concurrent.LinkedMap as LM
import Data.Concurrent.SkipListMap as SLM
import Data.Int
import Debug.Trace (traceEventIO)
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

{-# INLINE mkBenchSuite #-}
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

  -- Temp, hack to not overstress the O(N) insert version:
  let filt ls | name == "LMap" = filter (< 100000) ls
              | otherwise = ls
  
  let sizes    = filt [ 10^e | e <- [0,1,2,3,4::Int]]
      -- We need bigger sizes in parallel.. amortize forkJoin
      parSizes :: [Int]
      parSizes = filt [ 10000, 100000, 500000 ]
  numCap <- getNumCapabilities
--  let splits = 8 * numCap -- OVERPARTITION
  let splits = numCap -- 1-1 thread per core
  putStrLn $ "Running benchmarks for "++show numCap++" threads"

  -- rep performGC 5 -- For pushing data to oldest generation.
  -- putStrLn "Done preallocating."

  let forkNFill elems = do
        mp <- C.new :: IO (mp Key Val)
        let quota :: Int64
            quota = fromIntegral (elems `quot` splits)
        forkJoin splits (\chunk -> do 
                          let offset = fromIntegral (chunk * fromIntegral quota) 
                          putStrLn ("Running loop iters "++show (offset, (offset+quota-1)))
                          for_ offset (offset+quota-1) $
                            \ !i -> C.insert mp i i)
        return mp
  
  return $
    [ PreBench (name++"/new") (rep (C.new :: IO (mp Key Val))) ] ++
    [ PreBench (name++"/insert-"++show n) (rep (fillN n))
    | n <- sizes ] ++ 

    -- Parallel benchmarks:
    [ PreBench (name++"/parBarrier-insert+size"++show elems)
      -- Here we have the sequential loop on the outside, barriers for
      -- each criterion iteration:
      (rep (do mp <- forkNFill elems
               -- In the serial region, we ask what size it is:
               _ <- C.estimateSize mp
               return ()
               ))
    | elems <- parSizes ] ++
    
    [ PreBench (name++"/parBarrier-insert"++show elems)
      (rep (void$ forkNFill elems)) | elems <- parSizes ]
    

    -- Here we instead elide the barriers and pre-allocate the
    -- structures we will operate on.  We thus allow different
    -- criterion iterations to actually overlap in real time.  This
    -- models performance inside an application with abundant
    -- parallelism.
    


-- | Prebuild N empty copies of a structure so we can communicate
--   their locations once, and blast them all in parallel.
makeNfillN = do
  undefined

-- Just getting all the code in one place for debugging:
{-# NOINLINE forkJoin #-}   
-- | Run N copies of an IO action in parallel.  Pass in a number from
-- 0..N-1, letting the worker know which it is.
forkJoin :: Int -> (Int -> IO ()) -> IO ()
forkJoin num act = loop2 num []
 where
  act' n = do tid <- myThreadId
              traceEventIO$ "Worker "++show n++" starting on thread "++show tid
              putStrLn $ "Start action "++show n++" of "++show num
              act n
              tid2 <- myThreadId
              traceEventIO$ "Worker "++show n++" finished, on thread "++show tid
              putStrLn $ "End action "++show n++" of "++show num

  -- VERSION 1: This strategy makes things exception safe:
  loop 0 ls = mapM_ wait ls
  loop n ls = withAsyncOn (n-1) (act' (n-1)) $ \ asnc ->                  
               loop (n-1) (asnc:ls)

  -- VERSION 2: The less safe version:
  loop2 0 ls = mapM_ takeMVar ls
  loop2 n ls = do mv <- newEmptyMVar
                  _ <- forkOn (n-1) (do act' (n-1); putMVar mv ())
                  loop2 (n-1) (mv:ls)
  -- Both VER 1&2 are exhibiting problems with SLMap where the different workers
  -- take VASTLY different amounts of time.  There's still >2X par speedup, but
  -- the threadscope looks suspicious.

  -- VERSION 3: No pinning:  Just switching to this version slows us down a bit
  -- but not much.  However, it will allow overpartitioning.
  loop3 0 ls = mapM_ takeMVar ls
  loop3 n ls = do mv <- newEmptyMVar
                  _  <- forkIO (do act' (n-1); putMVar mv ())
                  loop3 (n-1) (mv:ls)


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
