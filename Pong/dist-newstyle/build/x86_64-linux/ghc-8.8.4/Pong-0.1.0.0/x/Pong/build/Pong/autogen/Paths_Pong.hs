{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_Pong (
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

bindir     = "/home/tyler/.cabal/bin"
libdir     = "/home/tyler/.cabal/lib/x86_64-linux-ghc-8.8.4/Pong-0.1.0.0-inplace-Pong"
dynlibdir  = "/home/tyler/.cabal/lib/x86_64-linux-ghc-8.8.4"
datadir    = "/home/tyler/.cabal/share/x86_64-linux-ghc-8.8.4/Pong-0.1.0.0"
libexecdir = "/home/tyler/.cabal/libexec/x86_64-linux-ghc-8.8.4/Pong-0.1.0.0"
sysconfdir = "/home/tyler/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Pong_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Pong_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Pong_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Pong_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Pong_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Pong_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
