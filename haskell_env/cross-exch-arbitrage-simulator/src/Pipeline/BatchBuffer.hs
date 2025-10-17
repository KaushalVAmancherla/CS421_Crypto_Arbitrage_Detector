{-|
Module      : Pipeline.BatchBuffer
Description : Thread-safe buffer for batching cryptocurrency market data
Copyright   : (c) Kaushala Amancherla, 2025
License     : MIT

This module implements a concurrent buffer system for aggregating
real-time cryptocurrency market data from multiple exchanges. It employs Software
Transactional Memory (STM) to ensure thread safety and data consistency.

Key Features:
* Lock-free concurrent data structure using STM
* Priority-based batch processing using a min-heap
* Timestamp-based synchronization of exchange data
* Automatic batch completion detection
* Memory-efficient accumulator pattern
-}

module Pipeline.BatchBuffer where

-- Concurrency imports
import Control.Concurrent.STM 
  ( STM
  , TVar
  , newTVarIO
  , readTVar
  , writeTVar
  , modifyTVar'
  )
import Control.Monad (when)

-- Data structure imports
import Data.Heap (MinPrioHeap)
import Data.Map.Strict (Map)
import Data.Text (Text)

-- Qualified imports
import Data.Heap qualified as H
import Data.Map.Strict qualified as Map
import Data.Text qualified as T

-- Project imports
import Model.Snapshot (Snapshot(..))  -- Provides datetime, exchange fields

-- | Accumulator buckets: timestamp-text → (exchange → snapshot)
-- | A two-level map structure for accumulating exchange snapshots:
-- * Outer Map: Timestamp -> Inner Map
-- * Inner Map: Exchange Name -> Exchange Snapshot
--
-- This structure allows efficient lookup and aggregation of snapshots
-- from multiple exchanges at specific timestamps.
type Buckets = Map Text (Map Text Snapshot)

-- | Handle holding *both* STM variables plus the exchange count
-- | Thread-safe buffer for collecting and synchronizing exchange snapshots.
-- Uses STM for concurrent access and a priority heap for ordered processing.
data BatchBuffer = BatchBuffer
  { -- | Total number of exchanges being monitored
    totalExs :: !Int,
    -- | Accumulator for incomplete batches, using STM for thread safety
    accumVar :: TVar Buckets,
    -- | Priority queue of completed batches, ordered by timestamp
    -- Each entry is (timestamp, [(exchange, snapshot)])
    heapVar :: TVar (MinPrioHeap Text [(Text, Snapshot)]),
    -- | Counter for active producer threads
    producersLeft :: TVar Int
  }

-- | Creates a new BatchBuffer for a specified number of exchanges.
-- Initializes all STM variables with empty states.
newBuffer :: Int  -- ^ Number of exchanges to monitor
         -> IO BatchBuffer
newBuffer n = do
  acc <- newTVarIO Map.empty            -- Empty accumulator
  hp <- newTVarIO H.empty               -- Empty priority queue
  producersLeft <- newTVarIO n          -- All producers active
  pure $ BatchBuffer n acc hp producersLeft

-- | Atomically inserts a snapshot into the buffer and manages batch completion.
-- When all exchanges for a timestamp are received, the batch is automatically
-- moved to the priority queue for processing.
insertSnapshot :: BatchBuffer  -- ^ Target buffer
              -> Snapshot     -- ^ New snapshot to insert
              -> STM ()
insertSnapshot (BatchBuffer totalExs accumVar heapVar _) snapshot = do
  buckets <- readTVar accumVar

  let -- Extract key fields from snapshot
      timestamp = datetime snapshot        -- e.g. "2025-04-22T12:03:00"
      snapshotExchange = exchange snapshot -- e.g. "Binance"

      -- Find or create bucket for this timestamp
      timestamp_bucket = Map.findWithDefault Map.empty timestamp buckets

      -- Add new snapshot to the exchange bucket
      exchange_bucket = Map.insert snapshotExchange snapshot timestamp_bucket

      -- Update main buckets map
      buckets_updated = Map.insert timestamp exchange_bucket buckets

  -- Update accumulator with new snapshot
  writeTVar accumVar buckets_updated

  -- Check if batch is complete (all exchanges received)
  when (Map.size exchange_bucket == totalExs) $ do
    -- Remove completed batch from accumulator
    writeTVar accumVar (Map.delete timestamp buckets_updated)

    -- Move completed batch to priority queue
    modifyTVar' heapVar (H.insert (timestamp, Map.toList exchange_bucket))

-- | Decrements the count of active producer threads.
-- Used to track when all producers have finished their work.
decrementProducers :: BatchBuffer  -- ^ Target buffer
                  -> STM ()
decrementProducers buffer = 
  modifyTVar' (producersLeft buffer) (\x -> x - 1)