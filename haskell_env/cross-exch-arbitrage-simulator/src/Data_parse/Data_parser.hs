module Data_parse.Data_parser where

import           Conduit                               (runConduitRes, (.|), sourceFile)
import qualified Data.Conduit.Combinators            as CC
import           Data.Conduit.Zstd                   (decompress)
import           Control.Concurrent                  (threadDelay)
import           Control.Concurrent.STM              (atomically, modifyTVar')
import           Data.Aeson                          (decode)
import qualified Data.ByteString.Lazy                as BL
import           Control.Monad                       (when)
import           Coordinator.Coordinator             (Coordinator, insertSnapshot, decrementProducers)
import           Data_parse.Snapshot                 (Snapshot)
import           Control.Monad.IO.Class   (liftIO)

-- Stream file x's snapshots into the coordinator
-- pausing `delayUS` microseconds between each snapshot injestion to simulate delta between API updating its data
streamFileHaskell :: Coordinator -> Int -> FilePath -> IO ()
streamFileHaskell coord delayUS fp = do
  runConduitRes $
    sourceFile fp
    .| decompress -- decompress zstd
    .| CC.linesUnboundedAscii
    .| CC.mapM_ (\strictBS -> --split byte stream into lines of strict ByteString
         case decode (BL.fromStrict strictBS) of --convert strict ByteString to lazy
           Nothing       -> pure () --skip malformed lines
           Just snapshot -> do -- if line is successfuly parsed into a Snapshot object, hand Snapshot into our coordinator
             liftIO $ atomically (insertSnapshot coord snapshot)
             liftIO $ threadDelay delayUS --simulate delta between API-side updates
       )
  atomically $ decrementProducers coord --once all lines are processed in the snapshot (total 1440), update coordinator to indicate this producer thread is finished 