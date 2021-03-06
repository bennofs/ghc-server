{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Server.Handler
  ( Handler, runHandler, withEnv, viewConfig
  , Server, runServer
  , Env(), workingDirectory, cabalTargets, cabalEnabled
  , Config(), errors, setupConfigDirty, packageDBDirty
  , resolveFile, watchPackageDB, unwatchPackageDB, onlyWatchPackageDBs
  ) where

import qualified Config
import           Control.Applicative
import           Control.Concurrent.MVar
import qualified Control.Exception.Lifted as E hiding (Handler)
import           Control.Lens
import           Control.Monad
import           Control.Monad.Base
import           Control.Monad.Extra
import qualified Control.Monad.Identity as I
import           Control.Monad.Morph
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Control
import qualified Data.DList as DL
import           Data.Default
import           Data.IORef
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Set as S
import           Distribution.Client.Dynamic hiding (packageDBs)
import qualified DynFlags
import qualified Exception
import qualified GHC
import qualified GHC.Paths
import qualified GhcMonad
import qualified HscTypes
import qualified MonadUtils
import qualified Panic
import           Server.Errors
import qualified Server.TargetMap as TM
import qualified SysTools
import           System.Directory
import           System.FilePath
import           System.INotify
import           System.Info

-- | This type represents the current environment of the server. The environment stays constant in a single request, but it might
-- change between requests.
data Env = Env
  { -- | We save the working directory of the client so we can resolve the paths sent from the client and also give good error messages.
    _workingDirectory :: !FilePath

    -- | IO actions that disable watching the given package database.
  , _packageDBWatchers :: !(M.Map PackageDB (IO ()))

    -- | The targets from the cabal file, if we loaded it.
  , _cabalTargets :: TM.TargetMap

    -- | Should we enable cabal support?
  , _cabalEnabled :: Bool
  }
makeLenses ''Env

instance Default Env where def = Env def M.empty TM.empty True

-- | This type represents the configuration of the server. It stays constant for the whole run time of the server, and does not
-- change between requests.
data Config = Config
  { _ghcSession        :: !(IORef HscTypes.HscEnv)
  , _errors            :: !(MVar (DL.DList GHCError))
  , _setupConfigDirty  :: !(MVar ())
  , _packageDBDirty    :: !(MVar ())
  , _inotify           :: !INotify
  }
makeLenses ''Config

-- | A type alias for the GhcServerM core, because that type is needed often in constraints. The core is the type of the
-- wrapped monad.
type ServerCoreM (t :: * -> (* -> *) -> * -> *) = t Env (ReaderT Config IO)

-- | A monad that allows access to the config and to the environment. The monad transformer for storing the environment can
-- be changed, so that both immutable (using ReaderT for t) and mutable (using StateT for t) environments are possible.
-- This monad can also be used for performing GHC actions.
newtype GhcServerM t a = GhcServerM { unGhcServerM :: ServerCoreM t a}

deriving instance Functor (ServerCoreM t) => Functor (GhcServerM t)
deriving instance Applicative (ServerCoreM t) => Applicative (GhcServerM t)
deriving instance Monad (ServerCoreM t) => Monad (GhcServerM t)
instance (Monad (ServerCoreM t), MonadTrans (t Env)) => MonadIO (GhcServerM t) where
  liftIO = GhcServerM . lift . lift
instance (Applicative (ServerCoreM t), Monad (ServerCoreM t), MonadTrans (t Env)) => MonadBase IO (GhcServerM t) where
  liftBase = liftIO

instance (MonadTrans (t Env), MonadBaseControl IO (ServerCoreM t)) => MonadBaseControl IO (GhcServerM t) where
  data StM (GhcServerM t) a = StServer (StM (ServerCoreM t) a)
  restoreM (StServer s) = GhcServerM $ restoreM s
  liftBaseWith f = GhcServerM $ liftBaseWith (\g -> f $ liftM StServer . g . unGhcServerM)

#if __GLASGOW_HASKELL__ < 708
instance MonadIO (GhcServerM t) => MonadUtils.MonadIO (GhcServerM t) where
  liftIO = Control.Monad.Reader.liftIO
#endif

instance (MonadTrans (t Env), MonadBaseControl IO (ServerCoreM t)) => Exception.ExceptionMonad (GhcServerM t) where
  gcatch = E.catch
  gmask f = E.mask $ \g -> f g -- Cannot be eta-reduced because of higher rank types

#if __GLASGOW_HASKELL__ >= 706
instance GhcMonad.GhcMonad (GhcServerM t) => DynFlags.HasDynFlags (GhcServerM t) where
  getDynFlags = GhcMonad.getSessionDynFlags
#endif

instance (MonadTrans (t Env), MonadBaseControl IO (ServerCoreM t), MonadIO (ServerCoreM t)) => GhcMonad.GhcMonad (GhcServerM t) where
  getSession = viewConfig ghcSession >>= liftIO . readIORef
  setSession s = viewConfig ghcSession >>= liftIO . flip writeIORef s

-- | Execute an action on the current environment in the GhcServerM monad.
withEnv :: MFunctor (t Env) => t Env I.Identity a -> GhcServerM t a
withEnv = GhcServerM . hoist (return . I.runIdentity)

-- | View the part of the config viewed by the given Getting in the GhcServerM monad.
viewConfig :: MonadTrans (t Env) => Getting a Config a -> GhcServerM t a
viewConfig = GhcServerM . lift . view

-- | The Handler monad is used for the functions that implement commands (like compile / info / ...). It has no mutable state,
-- the environment is frozen. It also keeps a reference to the global server configuration.
type Handler = GhcServerM ReaderT

-- | The Server monad is used by the server itself. The environment is mutable, whereas the server configuration is not.
type Server = GhcServerM StateT

-- | Run a Handler in the server monad, by taking the current environment and freezing it.
runHandler :: Handler a -> Server a
runHandler (GhcServerM h) = do
  s <- withEnv get
  r <- GhcServerM $ lift $ runReaderT h s
  dflags <- GHC.getSessionDynFlags
  MonadUtils.liftIO $ SysTools.cleanTempFiles dflags
  MonadUtils.liftIO $ SysTools.cleanTempDirs dflags
  return r

-- | Run the server monad. This takes care of running the ghc monad. It does not set exceptions handlers, however.
runServer :: Server a -- ^ The action to run
          -> IO a
runServer s = do
  errs <- newEmptyMVar
  ses <- newIORef (Panic.panic "empty session")
  setupConfDirty <- newMVar ()
  pkgDBDirty     <- newEmptyMVar

  let stopWatching = do
        finalizers <- withEnv $ uses packageDBWatchers M.elems
        liftIO $ sequence_ finalizers >> putStrLn "Shutdown of Server monad finished."

  putStrLn $ "Using libdir: " ++ GHC.Paths.libdir
  withINotify $ \ino -> do
    res <- flip runReaderT (Config ses errs setupConfDirty pkgDBDirty ino) $ flip evalStateT def $ unGhcServerM $ flip E.finally stopWatching $ do
      GHC.initGhcMonad $ Just GHC.Paths.libdir
      dflags <- GHC.getSessionDynFlags >>= GHC.setSessionDynFlags >> GHC.getSessionDynFlags -- Init the session
      mapM_ watchPackageDB [GlobalDB, UserDB]
      watchCabal setupConfDirty
      GHC.defaultCleanupHandler dflags s

    res <$ putStrLn "Exiting runServer"

-- | Resolve a relative file name, producing an absolute file name.
resolveFile :: FilePath -> Handler FilePath
resolveFile f = do
  pwd <- withEnv $ view workingDirectory
  return $ pwd </> f

-- | Get the file or directory that a PackageDB refers to
packageDBPath :: GHC.GhcMonad m => PackageDB -> m (Maybe FilePath)
packageDBPath GlobalDB = fmap (Just . DynFlags.systemPackageConfig) GHC.getSessionDynFlags
packageDBPath (SpecificDB path) = return $ Just path
packageDBPath UserDB = MonadUtils.liftIO $ do
  appdir <- getAppUserDataDirectory "ghc"
  let dir = appdir </> (arch ++ '-':os ++ '-':Config.cProjectVersion)
      pkgconf = dir </> "package.conf.d"
  dirExists <- doesDirectoryExist pkgconf
  return $ if dirExists then Just pkgconf else Nothing

-- | Perform an action only when the given path exists. The action returns
-- another action that will be performed when the path is deleted again.
onExists :: INotify -> Maybe Bool -> FilePath -> IO (IO ()) -> IO (IO ())
onExists ino isDir path action = do
  wd <- getCurrentDirectory
  let components = flip resolveRelative [] $ map dropTrailingPathSeparator $ splitPath $ if isAbsolute path then path else wd </> path
      resolveRelative ("..":ps) (_:rs) = resolveRelative ps rs
      resolveRelative ("..":ps) []     = resolveRelative ps []
      resolveRelative ("." :ps) rs     = resolveRelative ps rs
      resolveRelative (p   :ps) rs     = resolveRelative ps (p:rs)
      resolveRelative []        rs     = rs
  let (name:parents) = components
      parent = joinPath $ reverse parents

  if null parents
    then action
    else onExists ino (Just True) parent $ do
      finalizerVar <- newEmptyMVar
      let finalize = tryTakeMVar finalizerVar >>= fromMaybe (return ())
          action'  = action >>= putMVar finalizerVar
      watcher <- addWatch ino [Move, Delete, Create] parent $ \ev -> case ev of
        (MovedIn d n _)  | maybe True (d ==) isDir && n == name -> finalize >> action'
        (Deleted d n)    | maybe True (d ==) isDir && n == name -> finalize
        (MovedOut d n _) | maybe True (d ==) isDir && n == name -> finalize
        (Created d n)    | maybe True (d ==) isDir && n == name -> finalize >> action'
        _                                       -> return ()
      when (fromMaybe True isDir) $ om when (doesDirectoryExist path) action'
      unless (fromMaybe False isDir) $ om when (doesFileExist path) action'
      return (removeWatch watcher >> finalize)

-- | Watch a path for changes. This will also watch all parents when the path
-- doesn't exist yet. When the filesystem object at the given path changes, the
-- given IO action will be executed.
watchPathChanges :: INotify -> Maybe Bool -> FilePath -> IO () -> IO (IO ())
watchPathChanges ino isDir path action = onExists ino isDir path $ do
  watcher <- addWatch ino [Modify, Move, Delete, Create] path $ const action
  (action >> removeWatch watcher) <$ action

-- | Watch a package db. This will register a handler, so that the packageDBDirty IORef is set
-- when the package db changed.
watchPackageDB :: PackageDB -> Server ()
watchPackageDB db = do
  path <- packageDBPath db
  case path of
    Nothing -> return ()
    Just path' -> do
      mans <- withEnv $ use packageDBWatchers
      packageDBChanged <- viewConfig packageDBDirty
      ino <- viewConfig inotify

      unless (M.member db mans) $ do
        man <- liftIO $ watchPathChanges ino Nothing path' $ void $ tryPutMVar packageDBChanged ()
        withEnv $ packageDBWatchers.at db ?= man

-- | Stop watching a package db. After calling this function, the packageDBDirty IORef won't be
-- set anymore when the DB is changed.
unwatchPackageDB :: PackageDB -> Server ()
unwatchPackageDB db = do
  finalize <- withEnv $ use $ packageDBWatchers.at db
  case finalize of
    Nothing -> return ()
    Just action -> do
      liftIO action
      withEnv $ packageDBWatchers.at db .= Nothing

-- | Only watch the given package databases. Package databases that are not yet watched are added to the watch list,
-- packages that are watched but are not in the argument list will be removed from the watch list.
onlyWatchPackageDBs :: [PackageDB] -> Server ()
onlyWatchPackageDBs dbs = do
  let dbsSet = S.fromList dbs
  man <- withEnv $ use packageDBWatchers
  let watchedDbs = M.keysSet man
  mapM_ unwatchPackageDB $ S.toList $ watchedDbs `S.difference` dbsSet
  mapM_ watchPackageDB   $ S.toList $ dbsSet `S.difference` watchedDbs

-- | Watch setup-config, and if dist directory doesn't exist yet, also watch for that directory.
watchCabal :: MVar () -> Server ()
watchCabal changed = do
  ino <- viewConfig inotify
  liftIO $ void $ watchPathChanges ino (Just False) "dist/setup-config" $ void $ tryPutMVar changed ()
