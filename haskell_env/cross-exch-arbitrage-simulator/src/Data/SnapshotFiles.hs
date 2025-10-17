{-- 
Returns the full file paths for the compressed snapshots in the given snapshot directory
--}

module Data.SnapshotFiles where

import System.Directory  (listDirectory)

getSnapshotFiles :: FilePath -> IO [FilePath]
getSnapshotFiles dir = do
  filenames <- listDirectory dir
  pure $ map (\name -> dir ++ "/" ++ name) filenames