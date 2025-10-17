{-|
Module      : Pipeline.ProcessBatch
Description : Parallel batch processing for cryptocurrency arbitrage detection
Copyright   : (c) Kaushala Amancherla, 2025
License     : MIT

This module implements a parallel processing pipeline for detecting arbitrage
opportunities across multiple cryptocurrency exchanges. It employs a MapReduce-style
approach to efficiently process large batches of market data:

1. Map Phase: Divides exchange snapshots into chunks for parallel processing
2. Process Phase: Each chunk identifies local best prices
3. Reduce Phase: Merges results to find global arbitrage opportunities
-}

module Pipeline.ProcessBatch where

-- Parallel processing imports
import Control.Parallel.Strategies (parMap, rdeepseq)

-- Data structure imports
import Data.List.Split (chunksOf)
import Data.Map.Strict (Map)
import Data.Text (Text)

-- Qualified imports
import Data.Map.Strict qualified as Map

-- Project imports
import Model.Snapshot (Snapshot(..), Tick(..))

-- | Creates a price map for a single exchange snapshot.
-- For each trading symbol, maps to a tuple containing:
-- 1. Buy side: (price, exchange)
-- 2. Sell side: (price, exchange)
-- Initially, both sides use the same price as a placeholder for the merge phase.
getSnapshotBestMap :: (Text, Snapshot) -> Map Text ((Double, Text), (Double, Text))
getSnapshotBestMap (exch, snap) =
  let ohlc_map = ohlc snap
   in Map.map (\t -> ((close t, exch), (close t, exch))) ohlc_map

-- | Merges two price pairs to find the best arbitrage opportunity.
-- For each trading pair:
-- * Selects the lower price for buying (bp = buy price, bx = buy exchange)
-- * Selects the higher price for selling (sp = sell price, sx = sell exchange)
mergeBest :: ((Double, Text), (Double, Text)) -> ((Double, Text), (Double, Text)) -> ((Double, Text), (Double, Text))
mergeBest ((bp1, bx1), (sp1, sx1)) ((bp2, bx2), (sp2, sx2)) =
  (if bp2 < bp1 then (bp2, bx2) else (bp1, bx1), if sp1 < sp2 then (sp2, sx2) else (sp1, sx1))

-- | Processes a chunk of exchange snapshots to find local best prices.
-- This function implements the "Map" phase of our MapReduce approach:
-- 1. Maps each snapshot to its best prices
-- 2. Merges all maps within the chunk to find local optimal prices
processChunk :: [(Text, Snapshot)] -> Map Text ((Double, Text), (Double, Text))
processChunk batchChunk =
  let exchMaps = map getSnapshotBestMap batchChunk  -- Create initial maps for each exchange
   in Map.unionsWith mergeBest exchMaps             -- Merge all exchange maps within the chunk

-- | Parallelizes the processing of exchange snapshots to find global arbitrage opportunities.
-- This is the main entry point for the parallel processing pipeline:
-- 1. Divides input into optimal chunk sizes based on available cores
-- 2. Processes chunks in parallel using Haskell's parallel strategies
-- 3. Merges results to find global best prices across all exchanges
parallelBestMap :: Int              -- ^ Number of CPU cores to utilize
                -> [(Text, Snapshot)] -- ^ List of (exchange, snapshot) pairs
                -> Map Text ((Double, Text), (Double, Text)) -- ^ Final map of symbols to best (buy, sell) prices
parallelBestMap numCores batch =
  let n = length batch
      -- Calculate optimal chunk size (ceiling division to ensure no snapshots are lost)
      chunkSize = max 1 $ (n + numCores - 1) `div` numCores
      -- Distribute snapshots across chunks
      chunks = chunksOf chunkSize batch
      -- Process chunks in parallel using deep evaluation strategy
      partials = parMap rdeepseq processChunk chunks
      -- Merge partial results to find global best prices
   in Map.unionsWith mergeBest partials