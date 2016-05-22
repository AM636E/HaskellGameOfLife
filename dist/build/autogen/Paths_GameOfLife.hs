module Paths_GameOfLife (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/zaz/.cabal/bin"
libdir     = "/home/zaz/.cabal/lib/x86_64-linux-ghc-7.8.4/GameOfLife-0.1.0.0"
datadir    = "/home/zaz/.cabal/share/x86_64-linux-ghc-7.8.4/GameOfLife-0.1.0.0"
libexecdir = "/home/zaz/.cabal/libexec"
sysconfdir = "/home/zaz/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "GameOfLife_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "GameOfLife_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "GameOfLife_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "GameOfLife_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "GameOfLife_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
