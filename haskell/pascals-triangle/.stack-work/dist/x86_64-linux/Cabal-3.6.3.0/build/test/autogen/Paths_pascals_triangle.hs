{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_pascals_triangle (
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
version = Version [1,5,0,9] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/samedov/Coding/FunctionalLanguages/haskell/pascals-triangle/.stack-work/install/x86_64-linux/0f527f3686ec6b307592821707c1e3f0fc28652d46f5e6f02b878d4ad23fb448/9.2.7/bin"
libdir     = "/home/samedov/Coding/FunctionalLanguages/haskell/pascals-triangle/.stack-work/install/x86_64-linux/0f527f3686ec6b307592821707c1e3f0fc28652d46f5e6f02b878d4ad23fb448/9.2.7/lib/x86_64-linux-ghc-9.2.7/pascals-triangle-1.5.0.9-HWWCufbvHtWAD9nv3xThTY-test"
dynlibdir  = "/home/samedov/Coding/FunctionalLanguages/haskell/pascals-triangle/.stack-work/install/x86_64-linux/0f527f3686ec6b307592821707c1e3f0fc28652d46f5e6f02b878d4ad23fb448/9.2.7/lib/x86_64-linux-ghc-9.2.7"
datadir    = "/home/samedov/Coding/FunctionalLanguages/haskell/pascals-triangle/.stack-work/install/x86_64-linux/0f527f3686ec6b307592821707c1e3f0fc28652d46f5e6f02b878d4ad23fb448/9.2.7/share/x86_64-linux-ghc-9.2.7/pascals-triangle-1.5.0.9"
libexecdir = "/home/samedov/Coding/FunctionalLanguages/haskell/pascals-triangle/.stack-work/install/x86_64-linux/0f527f3686ec6b307592821707c1e3f0fc28652d46f5e6f02b878d4ad23fb448/9.2.7/libexec/x86_64-linux-ghc-9.2.7/pascals-triangle-1.5.0.9"
sysconfdir = "/home/samedov/Coding/FunctionalLanguages/haskell/pascals-triangle/.stack-work/install/x86_64-linux/0f527f3686ec6b307592821707c1e3f0fc28652d46f5e6f02b878d4ad23fb448/9.2.7/etc"

getBinDir     = catchIO (getEnv "pascals_triangle_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "pascals_triangle_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "pascals_triangle_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "pascals_triangle_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "pascals_triangle_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "pascals_triangle_sysconfdir") (\_ -> return sysconfdir)




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
