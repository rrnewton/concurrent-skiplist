{-# LANGUAGE BangPatterns #-}

module Main where

import Criterion.Main
import Criterion.Types

import Data.Concurrent.LinkedMap as LM
import Data.Concurrent.SkipListMap as SLM

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

for_ :: Monad m => Int -> Int -> (Int -> m ()) -> m ()
for_ start end _ | start > end = error "start greater than end"
for_ start end fn = loop start
  where loop !i | i > end = return ()
                | otherwise = fn i >> loop (i+1)
{-# INLINE for_ #-}
