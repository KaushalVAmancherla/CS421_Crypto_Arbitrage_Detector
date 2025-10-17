{-|
Module      : Data.SnapshotFiles
Description : Utilities for locating snapshot files on disk
Copyright   : (c) Kaushala Amancherla, 2025
License     : MIT

Small helper function used by the simulator to discover the snapshot files created
by the dataset builder.
-}

module Data.SnapshotFiles where

import System.Directory (listDirectory)
import System.FilePath ((</>))

-- | Return full file paths for the snapshot files in the specified directory.
getSnapshotFiles :: FilePath -> IO [FilePath]
getSnapshotFiles dir = do
  filenames <- listDirectory dir
  pure $ map (dir </>) filenames