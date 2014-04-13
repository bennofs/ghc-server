{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Server.Configure
  ( loadCabal
  , reloadPkgDBs
  , initFlags
  , addFlags
  , resetFlags
  , addPackageDB
  , onlyPackageDBs
  , withFileOptions
  ) where


import           Control.Applicative
import           Control.Concurrent.MVar
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.List
import           Data.Monoid
import qualified Data.Text as T
import qualified ObjLink
import System.FilePath
#if __GLASGOW_HASKELL__ >= 706
import           Data.Time
#else
import           System.Time
#endif
import           Data.Version
import           Distribution.Client.Dynamic hiding (includeDirs)
import qualified DynFlags
import qualified Exception
import qualified GHC
import qualified GHC.Paths
import qualified GhcMonad
import qualified HscTypes
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
  let dbs = getPackageDatabases dflags'
  liftIO $ putStrLn $ "Watching package databases: " ++ show dbs
  onlyWatchPackageDBs dbs
  Nothing <$ GHC.setSessionDynFlags dflags'
    { GHC.pkgDatabase = Nothing
    }

  where handler e = return $ Just $ GHC.showGhcException e ""

-- | Add a package database. This database will be added to the top of the stack, so it will override
-- all previous package databases when there is an overlap.
addPackageDB :: PackageDB -> Server ()
addPackageDB db = do
  watchPackageDB db
  dflags <- GHC.getSessionDynFlags
  void $ GHC.setSessionDynFlags $ addPackageDB' db dflags
    { GHC.pkgDatabase = Nothing
    }

-- | Only use the given package databases, no other ones. Removes all databases
-- that are not in the list, adds the missing ones.
onlyPackageDBs :: [PackageDB] -> Server ()
onlyPackageDBs dbs = do
  liftIO $ putStrLn $ "Using only package DBs: " ++ show dbs
  onlyWatchPackageDBs dbs
  dflags <- GHC.getSessionDynFlags
  void $ GHC.setSessionDynFlags $ flip (foldl' $ flip addPackageDB') dbs $ clearPackageDBs $ dflags
    { GHC.pkgDatabase = Nothing
    }

-- | Find all object files in the given directory, recursively.
findObjectFiles :: FilePath -> IO [FilePath]
findObjectFiles dir = do
  exists <- doesDirectoryExist dir
  if not exists
    then return []
    else do
      contents <- map (dir </>) . filter (not . (`elem` [".", ".."])) <$> getDirectoryContents dir
      files <- filter (".o" `isSuffixOf`) <$> filterM doesFileExist contents
      files' <- filterM doesDirectoryExist contents >>= mapM findObjectFiles
      return $ concat $ files : files'

-- | Load settings from the cabal project. This assumes that the file "dist/setup-config" exists and the
-- current directory is the root of the project.
loadCabal :: Producer Message Server ()
loadCabal = do
  status "loadCabal" 1 "(Re)loading cabal project settings"
  resetFlags
  void $ lift $ viewConfig setupConfigDirty >>= liftIO . tryTakeMVar

  (pkgDBs, tgts) <- liftIO $ runQuery ((,) <$> packageDBs <*> on localPkgDesc targets) "dist/setup-config"
  tgts' <- liftIO $ mapM TM.canonicalizeTarget tgts
  lift $ withEnv $ cabalTargets .= TM.fromTargets tgts'

  status "loadCabal" 2 $ "Using package databases: " <> T.pack (show pkgDBs)
  lift $ onlyPackageDBs pkgDBs

  includeDirs <- liftIO $ filterM doesFileExist ["dist/build/autogen/cabal_macros.h"]
  status "loadCabal" 2 $ "Including files: " <> T.pack (unwords includeDirs)

  importDirs <- liftIO $ filterM doesDirectoryExist $ "dist/build/autogen" : "dist/build" : [p | tgt <- tgts, isLibrary tgt, p <- sourceDirs tgt]
  status "loadCabal" 2 $ "Using import search path: " <> T.pack (unwords importDirs)

  objs <- liftIO $ fmap concat $ mapM (findObjectFiles . ("dist/build" </>)) $ filter (/= ".") $ filter isLibrary tgts >>= sourceDirs
  status "loadCabal" 2 $ "Found object files: " <> T.pack (unwords objs)

  let deps = [n ++ '-':showVersion v | (n, Just v) <- concatMap dependencies tgts]
  status "loadCabal" 2 $ "Using dependencies: " <> T.pack (unwords deps)

  status "loadCabal" 2 "Applying settings"
  dflags <- lift GHC.getSessionDynFlags
  s <- liftIO $ mapM_ ObjLink.loadObj objs >> ObjLink.resolveObjs
  case s of
    GHC.Succeeded -> status "loadCabal" 3 "Link success"
    GHC.Failed -> status "loadCabal" 3 "Link failure"
  lift $ void $ GHC.setSessionDynFlags $ (DynFlags.dopt_set dflags GHC.Opt_HideAllPackages)
    { DynFlags.packageFlags = map DynFlags.ExposePackage deps
    , DynFlags.importPaths = importDirs
    , DynFlags.settings = (DynFlags.settings dflags)
        { DynFlags.sOpt_P = reverse $ concatMap (\x -> ["-include", x]) includeDirs
        }
    }

-- | Like setSessionDynFlags, but does not reload the package database / state.
setSessionDynFlags' :: (MonadIO m, GHC.GhcMonad m) => GHC.DynFlags -> m ()
setSessionDynFlags' dflags = do
  GhcMonad.modifySession (\h -> h
    { HscTypes.hsc_dflags = dflags
#if __GLASGOW_HASKELL__ >= 706
    , HscTypes.hsc_IC = (HscTypes.hsc_IC h){ HscTypes.ic_dflags = dflags }
#endif
    })
  invalidateModSummaryCache

invalidateModSummaryCache :: GHC.GhcMonad m => m ()
invalidateModSummaryCache = GhcMonad.modifySession $ \h -> h { HscTypes.hsc_mod_graph = map inval (HscTypes.hsc_mod_graph h) }
 where
#if __GLASGOW_HASKELL__ >= 706
  inval ms = ms { HscTypes.ms_hs_date = addUTCTime (-1) (HscTypes.ms_hs_date ms) }
#else
  inval ms = ms { HscTypes.ms_hs_date = addToClockTime (TimeDiff 0 0 0 0 0 0 (-1)) (HscTypes.ms_hs_date ms) }
#endif

-- | Loads the cabal options for a given file. The argument should be an absolute file name.
withFileOptions :: FilePath -> Handler a -> Producer Message Handler a
withFileOptions path action = do
  status "loadFileOptions" 2 $ "Loading cabal options for file " <> T.pack (show path) <> " ..."
  tgts <- lift $ withEnv $ views cabalTargets $ TM.lookupBest path

  let usableOpt ('-':'O':_) = False
      usableOpt "-rtsopts" = False
      usableOpt _ = True

  let imps = concatMap sourceDirs tgts
      opts = filter usableOpt $ concatMap (ghcOptions <> (map ("-X" ++) . extensions)) tgts

  status "loadFileOptions" 3 $ "Adding import search path: " <> T.pack (unwords imps)
  status "loadFileOptions" 3 $ "Adding ghc options: " <> T.pack (unwords opts)

  dflags <- lift GHC.getSessionDynFlags
  (dflags', _, _) <- GHC.parseDynamicFlags dflags $ map GHC.noLoc opts

  lift $ void $ setSessionDynFlags' $ dflags'
    { DynFlags.importPaths = DynFlags.importPaths dflags' ++ imps
    , DynFlags.optLevel = 0
    }

  lift $ action `GHC.gfinally` setSessionDynFlags' dflags

-- | Reload the package database. You should call this function when new packages have been installed.
reloadPkgDBs :: Producer Message Server ()
reloadPkgDBs = do
  status "reloadPkgDBs" 1 "Reloading package databases ..."
  lift $ do
    dflags <- GHC.getSessionDynFlags
    void $ viewConfig packageDBDirty >>= liftIO . tryTakeMVar
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
    viewConfig setupConfigDirty >>= void . liftIO . flip tryPutMVar ()
    runHandler initFlags

-- Now the ugly stuff, for compat with GHC 7.4

#if __GLASGOW_HASKELL__ >= 706
getPackageDatabases :: DynFlags.DynFlags -> [PackageDB]
getPackageDatabases dflags = map fromGHCPkgDB $ DynFlags.extraPkgConfs dflags [DynFlags.UserPkgConf, DynFlags.GlobalPkgConf]
  where fromGHCPkgDB :: DynFlags.PkgConfRef -> PackageDB
        fromGHCPkgDB (DynFlags.PkgConfFile x) = SpecificDB x
        fromGHCPkgDB DynFlags.UserPkgConf = UserDB
        fromGHCPkgDB DynFlags.GlobalPkgConf = GlobalDB

addPackageDB' :: PackageDB -> DynFlags.DynFlags -> DynFlags.DynFlags
addPackageDB' db dflags = dflags { GHC.extraPkgConfs = (toGHCPkgDB db:) . GHC.extraPkgConfs dflags }
  where toGHCPkgDB :: PackageDB -> DynFlags.PkgConfRef
        toGHCPkgDB (SpecificDB x) = DynFlags.PkgConfFile x
        toGHCPkgDB UserDB = DynFlags.UserPkgConf
        toGHCPkgDB GlobalDB = DynFlags.GlobalPkgConf

clearPackageDBs :: DynFlags.DynFlags -> DynFlags.DynFlags
clearPackageDBs dflags = dflags { GHC.extraPkgConfs = const [] }
#else
getPackageDatabases :: DynFlags.DynFlags -> [PackageDB]
getPackageDatabases dflags = GlobalDB : mconcat
  [ [UserDB | DynFlags.dopt DynFlags.Opt_ReadUserPackageConf dflags]
  , map SpecificDB $ DynFlags.extraPkgConfs dflags
  ]

addPackageDB' :: PackageDB -> DynFlags.DynFlags -> DynFlags.DynFlags
addPackageDB' GlobalDB       dflags = dflags  -- The global package db is always enabled in GHC 7.4
addPackageDB' UserDB         dflags = DynFlags.dopt_set dflags DynFlags.Opt_ReadUserPackageConf
addPackageDB' (SpecificDB x) dflags = dflags { GHC.extraPkgConfs = x : GHC.extraPkgConfs dflags }

clearPackageDBs :: DynFlags.DynFlags -> DynFlags.DynFlags
clearPackageDBs dflags = (DynFlags.dopt_unset dflags DynFlags.Opt_ReadUserPackageConf) { GHC.extraPkgConfs = [] }
#endif
