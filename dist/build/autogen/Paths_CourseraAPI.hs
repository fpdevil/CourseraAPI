module Paths_CourseraAPI (
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

bindir     = "/Users/sampathsingamsetty/.cabal/bin"
libdir     = "/Users/sampathsingamsetty/.cabal/lib/x86_64-osx-ghc-7.10.3/CourseraAPI-0.1.0.0-0bVxQcuwGn4IliFHJ5S5oM"
datadir    = "/Users/sampathsingamsetty/.cabal/share/x86_64-osx-ghc-7.10.3/CourseraAPI-0.1.0.0"
libexecdir = "/Users/sampathsingamsetty/.cabal/libexec"
sysconfdir = "/Users/sampathsingamsetty/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "CourseraAPI_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "CourseraAPI_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "CourseraAPI_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "CourseraAPI_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "CourseraAPI_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
