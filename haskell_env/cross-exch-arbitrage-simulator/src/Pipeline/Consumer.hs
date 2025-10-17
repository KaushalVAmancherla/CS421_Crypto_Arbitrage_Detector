{-|
Module      : Pipeline.Consumer
Description : Real-time cryptocurrency arbitrage opportunity consumer
Copyright   : (c) Kaushala Amancherla, 2025
License     : MIT

This module implements the consumer component of the arbitrage detection pipeline.
It processes batched market data from multiple cryptocurrency exchanges in real-time
to identify and log profitable trading opportunities.

Key Features:
* Real-time performance monitoring with timing analysis
* Efficient validation of arbitrage opportunities
* Structured logging of trading opportunities
-}

module Pipeline.Consumer where

-- Concurrency and parallelism imports
import Control.Concurrent.STM (STM, TVar, atomically, readTVar, retry, writeTVar)
import GHC.Conc (getNumCapabilities)

-- Data structure imports
import Control.Monad (forM_)
import Data.Map.Strict (Map, foldMapWithKey)
import Data.Text (Text)
import Data.Time.Clock (addUTCTime, diffUTCTime, getCurrentTime)
import System.IO (IOMode(AppendMode), withFile)

-- Qualified imports
import Data.Heap qualified as H
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.IO qualified as TIO

-- Project imports
import Model.Opportunity (Opportunity(..))
import Model.Snapshot (Snapshot)
import Pipeline.BatchBuffer (BatchBuffer(..))
import Pipeline.ProcessBatch (parallelBestMap)

-- | Atomically retrieves the next batch of market data from the buffer.
-- Uses STM for thread-safe access to the shared buffer.
-- Blocks until data is available or all producers have finished.
popNextBatch :: BatchBuffer -> STM (Maybe (Text, [(Text, Snapshot)]))
popNextBatch (BatchBuffer _ _ heapVar producersLeft) = do
  heap <- readTVar heapVar
  case H.view heap of
    Just ((ts, batch), heap') -> do
      writeTVar heapVar heap'  -- Update heap with top element removed
      return (Just (ts, batch))
    Nothing -> do
      left <- readTVar producersLeft
      case left of
        0 -> return Nothing  -- All producers have finished
        _ -> retry          -- Block until more data is available

-- | Validates potential arbitrage opportunities in a price map.
-- For each trading symbol, checks if there exists a profitable arbitrage
-- opportunity where the selling price exceeds the buying price.
validateMap :: Map Text ((Double, Text), (Double, Text))  -- ^ Map of symbols to (buy, sell) price pairs
           -> [Opportunity]                               -- ^ List of valid arbitrage opportunities
validateMap batch = foldMapWithKey go batch
  where
    go sym ((bp, bx), (sp, sx))
      | sp > bp = [Opportunity sym bx bp sx sp]  -- Valid arbitrage found
      | otherwise = []                           -- No profitable opportunity

-- | Processes a batch of market data to identify arbitrage opportunities.
-- Uses parallel processing to analyze market data and logs any found opportunities.
process :: FilePath                    -- ^ Output file path for logging
        -> Text                        -- ^ Timestamp of the batch
        -> [(Text, Snapshot)]         -- ^ List of (exchange, snapshot) pairs
        -> IO ()
process fp ts batch = do
  caps <- getNumCapabilities
  let -- Parallel processing to find best prices across exchanges
      bestMap = parallelBestMap (max 1 caps) batch
      -- Validate opportunities where sell price > buy price
      validatedMap = validateMap bestMap
      timestamp = T.pack (show ts)

  -- Log arbitrage opportunities with detailed information
  withFile fp AppendMode $ \h -> do
    TIO.hPutStrLn h timestamp

    case validatedMap of
      [] -> TIO.hPutStrLn h "  no arbitrage opportunities in this batch."
      xs -> do
        TIO.hPutStrLn h $ "  found " <> T.pack (show (length xs)) <> " opportunity(ies):"
        forM_ xs $ \(Opportunity sym exBuy pBuy exSell pSell) -> do
          let line = "    "
                  <> sym
                  <> "  buy $"
                  <> T.pack (show pBuy)
                  <> " on "
                  <> exBuy
                  <> "  sell $"
                  <> T.pack (show pSell)
                  <> " on "
                  <> exSell
          TIO.hPutStrLn h line

-- | Main consumer loop that processes batches of market data with timing controls.
-- Implements a real-time processing pipeline with performance monitoring.
runConsumer :: BatchBuffer           -- ^ Shared buffer containing market data batches
            -> Double                -- ^ Target delay between batch processing (seconds)
            -> FilePath             -- ^ Output file path for logging
            -> IO ()
runConsumer buffer delaySec out = go
  where
    go = do
      -- Wait for next batch (blocking operation)
      mBatch <- atomically (popNextBatch buffer)
      case mBatch of
        Nothing -> pure ()  -- All producers have finished
        Just (ts, batch) -> do
          arrival <- getCurrentTime
          putStrLn $ "BATCH: " <> show ts <> " ARRIVAL TIME: " <> show arrival

          -- Process batch and measure performance
          process out ts batch
          end <- getCurrentTime

          -- Calculate and report timing metrics
          let target = addUTCTime (realToFrac delaySec) arrival
              slack = diffUTCTime target end
              work_time = diffUTCTime end arrival

          -- Log performance metrics with clear status indicators
          let status
                | slack > 0 = "[END EARLY]"
                | slack < 0 = "[END WARN ]"
                | otherwise = "[ON TIME ]"
          putStrLn $ status
                  <> " target="    <> show target
                  <> " actual="    <> show end
                  <> (if slack > 0 then " rem=" else " delay=")
                  <> show (if slack > 0 then slack else negate slack)
                  <> " work_time=" <> show work_time

          go  -- Continue processing next batch