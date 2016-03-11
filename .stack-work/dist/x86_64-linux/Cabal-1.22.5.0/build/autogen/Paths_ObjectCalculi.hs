module Paths_ObjectCalculi (
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

bindir     = "/home/shobhit/IdeaProjects/ObjectCalculi/.stack-work/install/x86_64-linux/lts-5.6/7.10.3/bin"
libdir     = "/home/shobhit/IdeaProjects/ObjectCalculi/.stack-work/install/x86_64-linux/lts-5.6/7.10.3/lib/x86_64-linux-ghc-7.10.3/ObjectCalculi-0.1.0.0-BomK7cNvSsqAdqssThutQm"
datadir    = "/home/shobhit/IdeaProjects/ObjectCalculi/.stack-work/install/x86_64-linux/lts-5.6/7.10.3/share/x86_64-linux-ghc-7.10.3/ObjectCalculi-0.1.0.0"
libexecdir = "/home/shobhit/IdeaProjects/ObjectCalculi/.stack-work/install/x86_64-linux/lts-5.6/7.10.3/libexec"
sysconfdir = "/home/shobhit/IdeaProjects/ObjectCalculi/.stack-work/install/x86_64-linux/lts-5.6/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "ObjectCalculi_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ObjectCalculi_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "ObjectCalculi_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ObjectCalculi_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ObjectCalculi_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
