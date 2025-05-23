{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_cross_exch_arbitrage_simulator (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude


#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/kaushalamancherla/cs421-honors-project/haskell_env/cross-exch-arbitrage-simulator/.stack-work/install/aarch64-osx/face4211ee83b8bf0dc7b07991170827810f6788695618f2566f771dd2dbaae6/9.8.4/bin"
libdir     = "/Users/kaushalamancherla/cs421-honors-project/haskell_env/cross-exch-arbitrage-simulator/.stack-work/install/aarch64-osx/face4211ee83b8bf0dc7b07991170827810f6788695618f2566f771dd2dbaae6/9.8.4/lib/aarch64-osx-ghc-9.8.4/cross-exch-arbitrage-simulator-0.1.0.0-DnL0YRzmE9R14gmEGDCHHa-cross-exch-arbitrage-simulator"
dynlibdir  = "/Users/kaushalamancherla/cs421-honors-project/haskell_env/cross-exch-arbitrage-simulator/.stack-work/install/aarch64-osx/face4211ee83b8bf0dc7b07991170827810f6788695618f2566f771dd2dbaae6/9.8.4/lib/aarch64-osx-ghc-9.8.4"
datadir    = "/Users/kaushalamancherla/cs421-honors-project/haskell_env/cross-exch-arbitrage-simulator/.stack-work/install/aarch64-osx/face4211ee83b8bf0dc7b07991170827810f6788695618f2566f771dd2dbaae6/9.8.4/share/aarch64-osx-ghc-9.8.4/cross-exch-arbitrage-simulator-0.1.0.0"
libexecdir = "/Users/kaushalamancherla/cs421-honors-project/haskell_env/cross-exch-arbitrage-simulator/.stack-work/install/aarch64-osx/face4211ee83b8bf0dc7b07991170827810f6788695618f2566f771dd2dbaae6/9.8.4/libexec/aarch64-osx-ghc-9.8.4/cross-exch-arbitrage-simulator-0.1.0.0"
sysconfdir = "/Users/kaushalamancherla/cs421-honors-project/haskell_env/cross-exch-arbitrage-simulator/.stack-work/install/aarch64-osx/face4211ee83b8bf0dc7b07991170827810f6788695618f2566f771dd2dbaae6/9.8.4/etc"

getBinDir     = catchIO (getEnv "cross_exch_arbitrage_simulator_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "cross_exch_arbitrage_simulator_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "cross_exch_arbitrage_simulator_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "cross_exch_arbitrage_simulator_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "cross_exch_arbitrage_simulator_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "cross_exch_arbitrage_simulator_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
