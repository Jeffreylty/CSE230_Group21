{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_brick_tac_toe (
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

bindir     = "/Users/liutianyi/CSE230_Group21/.stack-work/install/x86_64-osx/d0d798d42a289be443acfd3f7df40cefea9b5801d51919f4eebfe4bb6d32ebbd/8.10.7/bin"
libdir     = "/Users/liutianyi/CSE230_Group21/.stack-work/install/x86_64-osx/d0d798d42a289be443acfd3f7df40cefea9b5801d51919f4eebfe4bb6d32ebbd/8.10.7/lib/x86_64-osx-ghc-8.10.7/brick-tac-toe-0.1.0.0-I7sCUmqeZjv9dUrQYQuLUf-brick-tac-toe"
dynlibdir  = "/Users/liutianyi/CSE230_Group21/.stack-work/install/x86_64-osx/d0d798d42a289be443acfd3f7df40cefea9b5801d51919f4eebfe4bb6d32ebbd/8.10.7/lib/x86_64-osx-ghc-8.10.7"
datadir    = "/Users/liutianyi/CSE230_Group21/.stack-work/install/x86_64-osx/d0d798d42a289be443acfd3f7df40cefea9b5801d51919f4eebfe4bb6d32ebbd/8.10.7/share/x86_64-osx-ghc-8.10.7/brick-tac-toe-0.1.0.0"
libexecdir = "/Users/liutianyi/CSE230_Group21/.stack-work/install/x86_64-osx/d0d798d42a289be443acfd3f7df40cefea9b5801d51919f4eebfe4bb6d32ebbd/8.10.7/libexec/x86_64-osx-ghc-8.10.7/brick-tac-toe-0.1.0.0"
sysconfdir = "/Users/liutianyi/CSE230_Group21/.stack-work/install/x86_64-osx/d0d798d42a289be443acfd3f7df40cefea9b5801d51919f4eebfe4bb6d32ebbd/8.10.7/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "brick_tac_toe_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "brick_tac_toe_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "brick_tac_toe_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "brick_tac_toe_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "brick_tac_toe_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "brick_tac_toe_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)