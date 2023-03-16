{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_space_age (
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
version = Version [1,2,0,6] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/samedov/ELTE/FunctionalLanguages/haskell/space-age/.stack-work/install/x86_64-linux/d6942bdcf6d130afc483fa0fc3a5751daf060b851ea30ef7c92af1ff551d522b/9.0.2/bin"
libdir     = "/home/samedov/ELTE/FunctionalLanguages/haskell/space-age/.stack-work/install/x86_64-linux/d6942bdcf6d130afc483fa0fc3a5751daf060b851ea30ef7c92af1ff551d522b/9.0.2/lib/x86_64-linux-ghc-9.0.2/space-age-1.2.0.6-GEoXD3xqUAV3zYNaRCPnby"
dynlibdir  = "/home/samedov/ELTE/FunctionalLanguages/haskell/space-age/.stack-work/install/x86_64-linux/d6942bdcf6d130afc483fa0fc3a5751daf060b851ea30ef7c92af1ff551d522b/9.0.2/lib/x86_64-linux-ghc-9.0.2"
datadir    = "/home/samedov/ELTE/FunctionalLanguages/haskell/space-age/.stack-work/install/x86_64-linux/d6942bdcf6d130afc483fa0fc3a5751daf060b851ea30ef7c92af1ff551d522b/9.0.2/share/x86_64-linux-ghc-9.0.2/space-age-1.2.0.6"
libexecdir = "/home/samedov/ELTE/FunctionalLanguages/haskell/space-age/.stack-work/install/x86_64-linux/d6942bdcf6d130afc483fa0fc3a5751daf060b851ea30ef7c92af1ff551d522b/9.0.2/libexec/x86_64-linux-ghc-9.0.2/space-age-1.2.0.6"
sysconfdir = "/home/samedov/ELTE/FunctionalLanguages/haskell/space-age/.stack-work/install/x86_64-linux/d6942bdcf6d130afc483fa0fc3a5751daf060b851ea30ef7c92af1ff551d522b/9.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "space_age_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "space_age_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "space_age_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "space_age_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "space_age_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "space_age_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
