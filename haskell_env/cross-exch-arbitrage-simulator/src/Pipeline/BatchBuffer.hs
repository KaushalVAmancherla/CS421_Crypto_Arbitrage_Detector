module Pipeline.BatchBuffer where

import           Control.Concurrent.STM
import           Control.Monad                  (when)

import           Data.Map.Strict                (Map)
import qualified Data.Map.Strict                as Map

import           Data.Heap                (MinPrioHeap)
import qualified Data.Heap                as H

import           Data.Text                      (Text)
import qualified Data.Text                      as T

import           Model.Snapshot            (Snapshot (..))   -- fields -> datetime, exchange

-- | Accumulator buckets: timestamp-text → (exchange → snapshot)
type Buckets = Map Text (Map Text Snapshot)

-- | Handle holding *both* STM variables plus the exchange count
data BatchBuffer = BatchBuffer
  { totalExs :: !Int
  , accumVar :: TVar Buckets --hashMap of {timestamp : {exchange:snapshot}}
  , heapVar :: TVar (MinPrioHeap Text [(Text, Snapshot)]) --our heap stores datetime as our priority with the list of (exchange_name, Snapshot) pairs as the payload
  , producersLeft :: TVar Int --how many producer threads are still unfinished
  }

newBuffer :: Int -> IO BatchBuffer
newBuffer n = do
  acc <- newTVarIO Map.empty
  hp  <- newTVarIO H.empty
  productersLeft <- newTVarIO n
  pure $ BatchBuffer n acc hp productersLeft

insertSnapshot :: BatchBuffer -> Snapshot -> STM ()
insertSnapshot (BatchBuffer totalExs accumVar heapVar producersLeft) snapshot = do
  -- read the entire bucket map
  buckets <- readTVar accumVar

  let timestamp               = datetime snapshot        -- e.g. "2025-04-22T12:03:00"
      snapshotExchange        = exchange snapshot        -- e.g. "Binance"

      -- find the bucket with the same timestamp as the input snapshot
      -- if no such map exists for the given timestamp, return an empty Map
      timestamp_bucket        = Map.findWithDefault Map.empty timestamp buckets

      -- our new bucket exchange_bucket is {snapshotExchange: snapshot} added to timestamp_bucket
      exchange_bucket         = Map.insert snapshotExchange snapshot timestamp_bucket

      -- write exchange_bucket to buckets so the update will be captured
      buckets_updated         = Map.insert timestamp exchange_bucket buckets

  -- update accumulator first
  writeTVar accumVar buckets_updated

  -- if bucket complete, remove & push to heap
  -- bucket complete: exchange_bucket now has all n snapshots from n exchanges in it
  when (Map.size exchange_bucket == totalExs) $ do
    -- remove from our accumulator
    writeTVar accumVar (Map.delete timestamp buckets_updated)

    --add (timestamp, [(exchange,snapshot)]) to the heap
    modifyTVar' heapVar (H.insert (timestamp, Map.toList exchange_bucket))

decrementProducers :: BatchBuffer -> STM ()
decrementProducers buffer = modifyTVar' (producersLeft buffer) (\x -> x - 1)