{-# LANGUAGE RecordWildCards, BangPatterns #-}

-- | A module providing a common benchmark suite for mutable concurrent maps.

module Data.Concurrent.Map.Bench
       ()
       where

import Control.Exception
-- import Control.DeepSeq
import Data.Concurrent.LinkedMap as LM
import Data.Concurrent.SkipListMap as SLM
import System.Mem (performGC)
--------------------------------------------------------------------------------

data MapDict m k v =
  MapDict { new :: IO m
          , insert :: m -> k -> v -> IO ()
          }

-- | To avoid a dependence on Criterion, we deal in this simple
-- datatype which can easily be turned into a benchmark.
data PreBench = PreBench { name :: String
                         , batchRunner :: Int -> IO () }

mkBenchSuite :: String -> MapDict m Int Int -> IO [PreBench]
mkBenchSuite name MapDict{..} = do
  let fillN num = do
        mp <- new
        for_ 1 num $ \i -> insert mp i i
        return mp
{-          
          res <- LM.find lm (show i)
          case res of
           NotFound tok -> tryInsert tok i >> return ()
           _ -> return ()
-}
  
  -- Pre-allocate
  empty <- new 
  big <- fillN (10^4)
--  evaluate (rnf big)
  rep performGC 5
  putStrLn "Done preallocating."
  return $
    [ PreBench (name++"/new") (rep new) ] ++
    [ PreBench (name++"/insert"++show n) (rep (fillN n))
    | n <- [ 10^e | e <- [0,1,2,3,4]]
    ]
{-    
    -- Parallel benchmarks:
    [ PreBench (name++"/insert"++show n) (rep (fillN n))
    | n <- [ 10^e | e <- [0,1,2,3,4]]
    ]
-}


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


rep :: Monad m => m a -> Int -> m ()
rep m n = for_ 1 n (\_ -> m)

-- | Inclusive range.
for_ :: Monad m => Int -> Int -> (Int -> m a) -> m ()
for_ start end _ | start > end = error "start greater than end"
for_ start end fn = loop start
  where loop !i | i > end = return ()
                | otherwise = fn i >> loop (i+1)
{-# INLINE for_ #-}
