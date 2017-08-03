{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_BlockSort (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
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
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\toshi\\Documents\\Project\\BlockSort\\.stack-work\\install\\6c953ab2\\bin"
libdir     = "C:\\Users\\toshi\\Documents\\Project\\BlockSort\\.stack-work\\install\\6c953ab2\\lib\\x86_64-windows-ghc-8.0.2\\BlockSort-0.1.0.0-LbOyOVEhjAAKSfXaMTE9Mw"
dynlibdir  = "C:\\Users\\toshi\\Documents\\Project\\BlockSort\\.stack-work\\install\\6c953ab2\\lib\\x86_64-windows-ghc-8.0.2"
datadir    = "C:\\Users\\toshi\\Documents\\Project\\BlockSort\\.stack-work\\install\\6c953ab2\\share\\x86_64-windows-ghc-8.0.2\\BlockSort-0.1.0.0"
libexecdir = "C:\\Users\\toshi\\Documents\\Project\\BlockSort\\.stack-work\\install\\6c953ab2\\libexec"
sysconfdir = "C:\\Users\\toshi\\Documents\\Project\\BlockSort\\.stack-work\\install\\6c953ab2\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "BlockSort_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "BlockSort_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "BlockSort_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "BlockSort_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "BlockSort_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "BlockSort_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
