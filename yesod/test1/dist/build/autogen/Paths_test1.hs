module Paths_test1 (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,0,0], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/hobbes/.cabal/bin"
libdir     = "/home/hobbes/.cabal/lib/test1-0.0.0/ghc-7.0.2"
datadir    = "/home/hobbes/.cabal/share/test1-0.0.0"
libexecdir = "/home/hobbes/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "test1_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "test1_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "test1_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "test1_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
