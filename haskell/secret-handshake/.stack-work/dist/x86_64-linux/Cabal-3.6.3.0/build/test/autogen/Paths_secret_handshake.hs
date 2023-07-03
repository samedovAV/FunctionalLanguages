{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_secret_handshake (
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
version = Version [1,2,0,6] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/samedov/Coding/FunctionalLanguages/haskell/secret-handshake/.stack-work/install/x86_64-linux/0f527f3686ec6b307592821707c1e3f0fc28652d46f5e6f02b878d4ad23fb448/9.2.7/bin"
libdir     = "/home/samedov/Coding/FunctionalLanguages/haskell/secret-handshake/.stack-work/install/x86_64-linux/0f527f3686ec6b307592821707c1e3f0fc28652d46f5e6f02b878d4ad23fb448/9.2.7/lib/x86_64-linux-ghc-9.2.7/secret-handshake-1.2.0.6-HdqawkGe8kzAxvaZPtPenZ-test"
dynlibdir  = "/home/samedov/Coding/FunctionalLanguages/haskell/secret-handshake/.stack-work/install/x86_64-linux/0f527f3686ec6b307592821707c1e3f0fc28652d46f5e6f02b878d4ad23fb448/9.2.7/lib/x86_64-linux-ghc-9.2.7"
datadir    = "/home/samedov/Coding/FunctionalLanguages/haskell/secret-handshake/.stack-work/install/x86_64-linux/0f527f3686ec6b307592821707c1e3f0fc28652d46f5e6f02b878d4ad23fb448/9.2.7/share/x86_64-linux-ghc-9.2.7/secret-handshake-1.2.0.6"
libexecdir = "/home/samedov/Coding/FunctionalLanguages/haskell/secret-handshake/.stack-work/install/x86_64-linux/0f527f3686ec6b307592821707c1e3f0fc28652d46f5e6f02b878d4ad23fb448/9.2.7/libexec/x86_64-linux-ghc-9.2.7/secret-handshake-1.2.0.6"
sysconfdir = "/home/samedov/Coding/FunctionalLanguages/haskell/secret-handshake/.stack-work/install/x86_64-linux/0f527f3686ec6b307592821707c1e3f0fc28652d46f5e6f02b878d4ad23fb448/9.2.7/etc"

getBinDir     = catchIO (getEnv "secret_handshake_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "secret_handshake_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "secret_handshake_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "secret_handshake_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "secret_handshake_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "secret_handshake_sysconfdir") (\_ -> return sysconfdir)




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