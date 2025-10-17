-- src/Main.hs
module Main where

import           Control.Concurrent.Async    (async)
import           Control.Monad               (forM_, void)
import           System.Environment          (getArgs)
import           System.Exit                 (die)
import           System.FilePath             ((</>))
import           Data.Time                   (getCurrentTime, utctDay)
import           Data.Time.Format            (formatTime, defaultTimeLocale)
import           System.Directory            (createDirectoryIfMissing)
import           System.FilePath             (takeDirectory, (</>))

import           Data.SnapshotFiles (getSnapshotFiles)
import           Pipeline.BatchBuffer     (newBuffer)
import           Data.Producer      (streamSnapshot)
import           Pipeline.Consumer      (runConsumer)

-- Required flag: --UTC day (YYYY-MM-DD)
getDayRequired :: IO String
getDayRequired = do
  args <- getArgs
  case args of
    ["--day", d] -> pure d
    _            -> die "Usage: cross-exch-arbitrage-simulator --day YYYY-MM-DD"

main :: IO ()
main = do
  day <- getDayRequired

  let inDir       = "../../datasets/crypto_snapshot_data"
      dayDir      = inDir </> day

  files <- getSnapshotFiles dayDir

  let numExs  = length files
      delayMicro  = 1000           -- microseconds to simulate delta between API calls
      delaySec    = fromIntegral delayMicro / 1000000  -- microseconds to seconds
      out_fp = "../../outputs/arbitrage.log"

  -- create buffer
  buffer <- newBuffer numExs

  -- spawn n producer threads that stream snapshot per-minute data into the shared buffer
  forM_ files $ \fp ->
    void . async $ streamSnapshot buffer delayMicro fp

  -- create output directory if it doesn't exist
  createDirectoryIfMissing True (takeDirectory out_fp)

  -- start consumer
  runConsumer buffer delaySec out_fp