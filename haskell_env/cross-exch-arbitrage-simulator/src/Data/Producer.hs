{-|
Module      : Data.Producer
Description : Streaming helpers that feed exchange snapshots into the simulator
Copyright   : (c) Kaushala Amancherla, 2025
License     : MIT

This module contains the producer-side streaming logic used by the simulator.
Each producer reads an NDJSON/ndjson.zst file for a single exchange, decodes
per-minute snapshots, and inserts them into the shared `BatchBuffer`.

For reproducibility of timing-sensitive behavior, the producer sleeps a fixed
microsecond interval between snapshot insertions to emulate API update delays.
-}

module Data.Producer where

-- Conduit streaming primitives
import Conduit (runConduitRes, sourceFile, (.|))
import Data.Conduit.Combinators qualified as CC
import Data.Conduit.Zstd (decompress)

-- Concurrency
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)

-- STM
import Control.Concurrent.STM (atomically)

-- JSON and bytestrings
import Data.Aeson (decode)
import Data.ByteString.Lazy qualified as BL

-- Project imports
import Model.Snapshot (Snapshot)
import Pipeline.BatchBuffer (BatchBuffer, decrementProducers, insertSnapshot)

-- | Stream snapshots from a compressed NDJSON file into the shared buffer.
-- The function decompresses the file, reads it line-by-line, decodes each
-- JSON line into a 'Snapshot' and inserts it atomically into the buffer.
-- A fixed delay (in microseconds) is introduced between insertions to simulate
-- API update intervals for reproducible behavior.
streamSnapshot :: BatchBuffer  -- ^ Shared buffer to receive snapshots
               -> Int          -- ^ Delay between snapshots in microseconds
               -> FilePath     -- ^ Path to compressed NDJSON (.ndjson.zst)
               -> IO ()
streamSnapshot buffer delayUS fp = do
  runConduitRes $
    sourceFile fp
      .| decompress
      .| CC.linesUnboundedAscii
      .| CC.mapM_ handleLine

  -- Mark this producer as finished once file is fully processed
  atomically $ decrementProducers buffer

  where
    handleLine strictBS =
      case decode (BL.fromStrict strictBS) of
        Nothing -> pure () -- Skip malformed lines silently
        Just snapshot -> do
          -- Insert snapshot into buffer and simulate API timing
          liftIO $ atomically (insertSnapshot buffer snapshot)
          liftIO $ threadDelay delayUS