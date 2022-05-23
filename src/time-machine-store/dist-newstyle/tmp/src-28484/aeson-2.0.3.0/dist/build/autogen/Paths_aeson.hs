{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_aeson (
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
version = Version [2,0,3,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/vincent/.cabal/store/ghc-8.6.5/aeson-2.0.3.0-7a09bafe9ea18cf3b238eec9aa59cf9257edbe063d818dda35fbbc7e4241f972/bin"
libdir     = "/home/vincent/.cabal/store/ghc-8.6.5/aeson-2.0.3.0-7a09bafe9ea18cf3b238eec9aa59cf9257edbe063d818dda35fbbc7e4241f972/lib"
dynlibdir  = "/home/vincent/.cabal/store/ghc-8.6.5/aeson-2.0.3.0-7a09bafe9ea18cf3b238eec9aa59cf9257edbe063d818dda35fbbc7e4241f972/lib"
datadir    = "/home/vincent/.cabal/store/ghc-8.6.5/aeson-2.0.3.0-7a09bafe9ea18cf3b238eec9aa59cf9257edbe063d818dda35fbbc7e4241f972/share"
libexecdir = "/home/vincent/.cabal/store/ghc-8.6.5/aeson-2.0.3.0-7a09bafe9ea18cf3b238eec9aa59cf9257edbe063d818dda35fbbc7e4241f972/libexec"
sysconfdir = "/home/vincent/.cabal/store/ghc-8.6.5/aeson-2.0.3.0-7a09bafe9ea18cf3b238eec9aa59cf9257edbe063d818dda35fbbc7e4241f972/etc"

getBinDir     = catchIO (getEnv "aeson_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "aeson_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "aeson_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "aeson_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "aeson_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "aeson_sysconfdir") (\_ -> return sysconfdir)




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
