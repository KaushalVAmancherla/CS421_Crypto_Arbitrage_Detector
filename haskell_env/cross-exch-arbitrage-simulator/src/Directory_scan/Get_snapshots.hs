{-- 
Returns the full file paths for the compressed snapshots in the given snapshot directory
--}

module Directory_scan.Get_snapshots where

import System.Directory  (listDirectory)

getSnapshotFiles :: FilePath -> IO [FilePath]
getSnapshotFiles dir = do
  filenames <- listDirectory dir
  pure $ map (\name -> dir ++ "/" ++ name) filenames