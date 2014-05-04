module Paths_wwrrd (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/kieran/.cabal/bin"
libdir     = "/Users/kieran/.cabal/lib/wwrrd-0.1/ghc-7.6.3"
datadir    = "/Users/kieran/.cabal/share/wwrrd-0.1"
libexecdir = "/Users/kieran/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "wwrrd_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "wwrrd_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "wwrrd_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "wwrrd_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
