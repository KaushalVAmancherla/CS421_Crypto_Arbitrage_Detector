-- src/Main.hs
module Main where

import           Control.Concurrent.Async    (async, waitAnyCancel)
import           Control.Monad               (forM_,void)
import           Directory_scan.Get_snapshots (getSnapshotFiles)
import           Coordinator.Coordinator     (newCoordinator)
import           Data_parse.Data_parser      (streamFileHaskell)
import           QueueConsumer.Consumer      (consumer)

main :: IO ()
main = do
  files <- getSnapshotFiles "/Users/kaushalamancherla/cs421-honors-project/metadata/crypto_snapshot_data/2025-04-22"

  let numExs  = length files
      delayMicro  = 9000           -- microseconds to simulate delta between API calls
      delaySec    = fromIntegral delayMicro / 1000000  -- microseconds to seconds
      out_fp = "/Users/kaushalamancherla/cs421-honors-project/outputs/arbitrage.log"

  coord <- newCoordinator numExs

  -- spawn n producer threads 
  forM_ files $ \fp ->
    void . async $ streamFileHaskell coord delayMicro fp

  -- run consumer thread
  consumer coord delaySec out_fp