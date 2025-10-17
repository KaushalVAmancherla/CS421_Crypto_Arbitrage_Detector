{-|
Module      : Main
Description : Entry point for the Cross-Exchange Arbitrage Simulator
Copyright   : (c) Kaushala Amancherl, 2025
License     : MIT

This module serves as the main entry point for the cross-exchange arbitrage simulator.
It coordinates the producer-consumer architecture where multiple producers stream
cryptocurrency snapshot data from different exchanges into a shared buffer, while
a single consumer processes this data to detect arbitrage opportunities.
-}

module Main where

-- Standard library imports
import Control.Concurrent.Async (async)
import Control.Monad (forM_, void)
import Data.Time (getCurrentTime, utctDay)
import Data.Time.Format (defaultTimeLocale, formatTime)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath (takeDirectory, (</>))

-- Project imports
import Data.Producer (streamSnapshot)
import Data.SnapshotFiles (getSnapshotFiles)
import Pipeline.BatchBuffer (newBuffer)
import Pipeline.Consumer (runConsumer)

-- | Parse the required day parameter from command line arguments.
--   Expected format: --day YYYY-MM-DD
getDayRequired :: IO String
getDayRequired = do
  args <- getArgs
  case args of
    ["--day", d] -> pure d
    _ -> die "Usage: cross-exch-arbitrage-simulator --day YYYY-MM-DD"

main :: IO ()
main = do
  day <- getDayRequired

  let inDir = "../../datasets/crypto_snapshot_data"
      dayDir = inDir </> day

  -- Get list of snapshot files from all exchanges for the given day
  files <- getSnapshotFiles dayDir

  let numExs = length files
      -- Delay between simulated API calls (in microseconds)
      delayMicro = 1000
      -- Convert microseconds to seconds for consumer timing
      delaySec = fromIntegral delayMicro / 1000000
      out_fp = "../../outputs/arbitrage.log"

  -- Initialize shared buffer with capacity for all exchanges
  buffer <- newBuffer numExs

  -- Spawn producer threads - one per exchange
  -- Each producer streams snapshot data into the shared buffer
  forM_ files $ \fp ->
    void . async $ streamSnapshot buffer delayMicro fp

  -- Ensure output directory exists
  createDirectoryIfMissing True (takeDirectory out_fp)

  -- Start the consumer thread that processes batched data
  -- and identifies arbitrage opportunities
  runConsumer buffer delaySec out_fp