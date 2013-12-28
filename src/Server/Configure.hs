{-# LANGUAGE OverloadedStrings #-}
module Server.Configure 
  ( addFlags
  , loadCabal
  , reloadPkgDBs
  , resetFlags
  , initFlags
  , addPackageDB
  , onlyPackageDBs
  , loadFileOptions
  ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Monoid
import qualified Data.Text as T
import           Data.Version
import           Distribution.Client.Dynamic hiding (includeDirs)
import qualified DynFlags
import qualified Exception
import qualified GHC
import qualified GHC.Paths
import           Message
import           Pipes
import           Server.Errors
import           Server.Handler
import qualified Server.TargetMap as TM
import           System.Directory

initFlags :: Handler ()
initFlags = do
  dflags <- GHC.getSessionDynFlags
  (dflags', _, _) <- GHC.parseDynamicFlags dflags $ map GHC.noLoc ["-Wall", "-O0"]
  errs <- viewConfig errors
  void $ GHC.setSessionDynFlags dflags'
    { GHC.ghcLink = GHC.NoLink
    , GHC.hscTarget = GHC.HscInterpreted
    , GHC.log_action = collectErrors errs                   
    }

addFlags :: [String] -> Server (Maybe String)
addFlags flags = Exception.ghandle handler $ do
  dflags <- GHC.getSessionDynFlags
  (dflags', _, _) <- GHC.parseDynamicFlags dflags $ map GHC.noLoc $ ["-Wall", "-O0"] ++ flags
  let dbs = map fromGHCPkgDB $ DynFlags.extraPkgConfs dflags' [DynFlags.UserPkgConf, DynFlags.GlobalPkgConf]
  liftIO $ putStrLn $ "Watching package databases: " ++ show dbs
  onlyWatchPackageDBs dbs
  Nothing <$ GHC.setSessionDynFlags dflags'
    { GHC.pkgDatabase = Nothing
    }

  where handler e = return $ Just $ GHC.showGhcException e ""

-- | Convert a ghc package database to a package db
fromGHCPkgDB :: DynFlags.PkgConfRef -> PackageDB
fromGHCPkgDB (DynFlags.PkgConfFile x) = SpecificDB x
fromGHCPkgDB DynFlags.UserPkgConf = UserDB
fromGHCPkgDB DynFlags.GlobalPkgConf = GlobalDB

-- | Convert a package db to a ghc package database
toGHCPkgDB :: PackageDB -> DynFlags.PkgConfRef
toGHCPkgDB (SpecificDB x) = DynFlags.PkgConfFile x
toGHCPkgDB UserDB = DynFlags.UserPkgConf
toGHCPkgDB GlobalDB = DynFlags.GlobalPkgConf

-- | Add a package database. This database will be added to the top of the stack, so it will override
-- all previous package databases when there is an overlap.
addPackageDB :: PackageDB -> Server ()
addPackageDB db = do
  watchPackageDB db
  dflags <- GHC.getSessionDynFlags
  void $ GHC.setSessionDynFlags dflags
    { GHC.pkgDatabase = Nothing
    , GHC.extraPkgConfs = (toGHCPkgDB db:) . GHC.extraPkgConfs dflags
    }

-- | Only use the given package databases, no other ones. Removes all databases
-- that are not in the list, adds the missing ones.
onlyPackageDBs :: [PackageDB] -> Server ()
onlyPackageDBs dbs = do
  liftIO $ putStrLn $ "Using only package DBs: " ++ show dbs
  onlyWatchPackageDBs dbs
  dflags <- GHC.getSessionDynFlags
  void $ GHC.setSessionDynFlags dflags
    { GHC.pkgDatabase = Nothing
    , GHC.extraPkgConfs = const $ map toGHCPkgDB dbs
    }

-- | Load settings from the cabal project. This assumes that the file "dist/setup-config" exists and the
-- current directory is the root of the project.
loadCabal :: Producer Message Server ()
loadCabal = do
  status "loadCabal" 1 "(Re)loading cabal project settings"
  lift $ writeVar setupConfigDirty False
  (pkgDBs, tgts) <- liftIO $ runQuery ((,) <$> packageDBs <*> on localPkgDesc targets) "dist/setup-config"
  tgts' <- liftIO $ mapM TM.canonicalizeTarget tgts
  lift $ withEnv $ cabalTargets %= TM.union (TM.fromTargets tgts')

  status "loadCabal" 2 $ "Using package databases: " <> T.pack (show pkgDBs)
  lift $ onlyPackageDBs pkgDBs

  importDirs <- liftIO $ filterM doesDirectoryExist $ "dist/build/autogen" : [p | tgt <- tgts, name tgt == Library, p <- sourceDirs tgt]
  status "loadCabal" 2 $ "Using import search path: " <> T.pack (unwords importDirs)

  includeDirs <- liftIO $ filterM doesFileExist ["dist/build/autogen/cabal_macros.h"]
  status "loadCabal" 2 $ "Including files: " <> T.pack (unwords includeDirs)

  let deps = [n ++ '-':showVersion v | (n, Just v) <- concatMap dependencies tgts]
  status "loadCabal" 2 $ "Using dependencies: " <> T.pack (unwords deps)
  
  status "loadCabal" 2 "Applying settings"
  dflags <- lift GHC.getSessionDynFlags
  lift $ void $ GHC.setSessionDynFlags $ (DynFlags.dopt_set dflags GHC.Opt_HideAllPackages)
    { DynFlags.packageFlags = map DynFlags.ExposePackage deps 
    , DynFlags.importPaths = importDirs
    , DynFlags.settings = (DynFlags.settings dflags)
        { DynFlags.sOpt_P = reverse $ concatMap (\x -> ["-include", x]) includeDirs
        }
    }

-- | Loads the cabal options for a given file. The argument should be an absolute file name.
loadFileOptions :: FilePath -> Producer Message Handler ()
loadFileOptions path = do
  status "loadFileOptions" 2 $ "Loading cabal options for file " <> T.pack (show path) <> " ..."
  tgts <- lift $ withEnv $ views cabalTargets $ TM.lookupBest path
  
  let imps = concatMap sourceDirs tgts
      opts = concatMap ghcOptions tgts
  status "loadFileOptions" 3 $ "Using import search path: " <> T.pack (unwords imps)

  dflags <- lift GHC.getSessionDynFlags
  (dflags', _, _) <- GHC.parseDynamicFlags dflags $ map GHC.noLoc opts 

  lift $ void $ GHC.setSessionDynFlags $ dflags'
     { DynFlags.importPaths = DynFlags.importPaths dflags' ++ imps
     }

-- | Reload the package database. You should call this function when new packages have been installed.
reloadPkgDBs :: Producer Message Server ()
reloadPkgDBs = do
  status "reloadPkgDBs" 1 "Reloading package databases ..."
  lift $ do
    dflags <- GHC.getSessionDynFlags
    writeVar packageDBDirty False
    void $ GHC.setSessionDynFlags $ dflags
      { GHC.pkgDatabase = Nothing
      }

-- | Clear the state of GHC. This will erase all passed flags.
--
-- Note: This will re-read the package database.
resetFlags :: Producer Message Server ()
resetFlags = do 
  status "resetFlags" 1 "Resetting flags ..."
  lift $ do
    GHC.initGhcMonad $ Just GHC.Paths.libdir
    withEnv $ cabalTargets .= TM.empty
    withEnv $ compilerFlags .= []
    writeVar setupConfigDirty True
    runHandler initFlags
