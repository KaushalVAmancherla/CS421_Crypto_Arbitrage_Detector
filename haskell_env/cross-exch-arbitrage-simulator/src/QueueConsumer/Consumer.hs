{-# LANGUAGE OverloadedStrings #-}

module QueueConsumer.Consumer where

import           Control.Concurrent       (threadDelay)
import           Control.Concurrent.STM   (STM, TVar, atomically, readTVar, retry, writeTVar)
import           Control.Monad            (forM_, when)

import           Data.Heap                (MinPrioHeap)
import qualified Data.Heap                as H

import System.IO

import           Data.Text                (Text)
import qualified Data.Text       as T
import qualified Data.Text.IO    as TIO

import           Data_parse.Snapshot      (Snapshot)
import           ProcessBatch.ProcessBatch (parallelBestMap)
import           Coordinator.Coordinator  (Coordinator(..))  -- import the record and its heapVar

import Portfolio.Portfolio

import           Data.Map.Strict         (Map,foldMapWithKey)
import qualified Data.Map.Strict         as Map

import qualified Data.Text.Time as TT
import           Data.Time.Clock          (UTCTime, diffUTCTime, addUTCTime, getCurrentTime)

import GHC.Conc (getNumProcessors)

-- atomically pop from the Coordinator thread's priority queue to retrieve the complete batch for further processing
popNextBatch :: Coordinator -> STM (Maybe (Text, [(Text, Snapshot)]))
popNextBatch (Coordinator _ _ heapVar producersLeft) = do
  heap <- readTVar heapVar 
  case H.view heap of 
    Just ((ts, batch), heap') -> do --batch exists
      writeTVar heapVar heap' --pop top of heap
      return (Just (ts, batch))
    Nothing -> do
      left <- readTVar producersLeft
      case left of 
        0 -> return Nothing --if no producer threads are left, we are done with this entire process
        _ -> retry -- block until we can fetch a completed batch from the heap

{--
For each symbol where we have its lowest/highest price in its respective exchange, validate that sell price > buy price
and if so, write it to a ValidArbitrage object
--}
validateMap :: Map Text ((Double, Text), (Double, Text)) -> [ValidArbitrage]
validateMap batch = foldMapWithKey go batch
  where 
   go sym ((bp,bx), (sp,sx))
    | sp > bp = [ValidArbitrage sym bx bp sx sp]
    | otherwise = []

process :: FilePath -> Text -> [(Text,Snapshot)] -> IO ()
process fp ts batch = do
    numCores <- getNumProcessors
    let 
        bestMap = parallelBestMap numCores batch --parallel processing to find the "bestMap" (i.e. map of coin to lowest/highest price data)
        validatedMap = validateMap bestMap --validate bestMap to ensure we keep only valid arbitrages
        timestmap = (T.pack (show ts))

    -- write arbitrage opportunities to log file
    withFile fp AppendMode $ \h -> do
      TIO.hPutStrLn h timestmap

      case validatedMap of
        [] -> TIO.hPutStrLn h "  no arbitrage opportunities in this batch."
        xs -> do
          TIO.hPutStrLn h $ "  found " <> T.pack (show (length xs)) <> " opportunity(ies):"
          forM_ xs $ \(ValidArbitrage sym exBuy pBuy exSell pSell) -> do
            let line =  "    " <> sym
                    <> "  buy $"  <> T.pack (show pBuy)  <> " on " <> exBuy
                    <> "  sell $" <> T.pack (show pSell) <> " on " <> exSell
            TIO.hPutStrLn h line

loop :: UTCTime -> Coordinator -> Double -> FilePath -> IO ()
loop nextStart coord producerDelay out = do
  res <- atomically (popNextBatch coord) -- pop batch from heap
  case res of
    Nothing -> pure ()
    Just (ts, batch) -> do
      let scheduledEnd = addUTCTime (realToFrac producerDelay) nextStart -- when the current batch is supposed to end
      
      putStrLn $ "BATCH: " <> show ts

      process out ts batch
      end <- getCurrentTime

      let slack = diffUTCTime scheduledEnd end

      when (slack > 0) $ do
          -- threadDelay (floor (slack * 1e6))
          putStrLn $ "[END EARLY] | TARGET END: " <> show scheduledEnd <> " ACTUAL END: " <> show end <> " REM : " <> show slack

      when (slack < 0) $ do -- you ended processing for this batch when you should have already been processing the next batch
          putStrLn $ "[END WARN] | TARGET END: " <> show scheduledEnd <> " ACTUAL END: " <> show end <> " DELAY: " <> show (negate slack)

      putStrLn $ ""

      loop scheduledEnd coord producerDelay out -- recursively loop through processing batches in heap until no more remain

consumer :: Coordinator -> Double -> FilePath -> IO ()
consumer coord producerDelay out = do
  first <- atomically (popNextBatch coord)
  case first of
    Nothing -> pure ()
    Just (ts, batch) -> do
      now <- getCurrentTime -- wall clock
      let scheduledEnd  = addUTCTime (realToFrac producerDelay) now --when Batch 1 should start

      putStrLn $ "BATCH: " <> show ts
      process out ts batch

      end <- getCurrentTime
      let slack = diffUTCTime scheduledEnd end

      when (slack > 0) $ do
          -- threadDelay (floor (slack * 1e6))
          putStrLn $ "[END EARLY] | TARGET END: " <> show scheduledEnd <> " ACTUAL END: " <> show end <> " REM : " <> show slack

      when (slack < 0) $ do -- you ended processing for this batch when you should have already been processing the next batch
          putStrLn $ "[END WARN] | TARGET END: " <> show scheduledEnd <> " ACTUAL END: " <> show end <> " DELAY: " <> show (negate slack)

      putStrLn $ ""

      loop scheduledEnd coord producerDelay out