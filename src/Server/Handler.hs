{-# LANGUAGE StandaloneDeriving #-}
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
  , Env(), compilerFlags, workingDirectory, cabalTargets, cabalEnabled
  , Config(), errors, setupConfigDirty, packageDBDirty
  , resolveFile, watchPackageDB, unwatchPackageDB, onlyWatchPackageDBs
  , writeVar, readVar
  ) where

import qualified Config
import           Control.Applicative
import           Control.Concurrent.MVar
import           Control.Exception.Lifted hiding (Handler)
import           Control.Lens
import           Control.Monad
import           Control.Monad.Base
import           Control.Monad.Identity
import           Control.Monad.Morph
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Control
import qualified Data.DList as DL
import           Data.Default
import           Data.IORef
import qualified Data.Map as M
import qualified Data.Set as S
import           Distribution.Client.Dynamic hiding (packageDBs)
import qualified DynFlags
import qualified Exception
import qualified Filesystem.Path.CurrentOS as P
import qualified GHC
import qualified GHC.Paths
import qualified GhcMonad
import qualified HscTypes
import qualified MonadUtils
import qualified Panic
import           Server.Errors
import           Server.TargetMap
import qualified Server.TargetMap as TM
import           System.Directory
import           System.FSNotify
import           System.FilePath
import           System.Info

-- | This type represents the current environment of the server. The environment stays constant in a single request, but it might
-- change between requests.
data Env = Env
  { -- | This stores the flags to pass to GHC. This is not strictly neccessary, as the flags will also be stored in the DynFlags,
    -- but might be useful for giving better debug messages.
    _compilerFlags    :: ![String]
    
    -- | We save the working directory of the client so we can resolve the paths sent from the client and also give good error messages.
  , _workingDirectory :: !FilePath    

    -- | The watch managers for the package databases. 
  , _packageDBWatchers :: !(M.Map PackageDB WatchManager)

    -- | The targets from the cabal file, if we loaded it.
  , _cabalTargets :: TargetMap

    -- | Should we enable cabal support?
  , _cabalEnabled :: Bool
  }
makeLenses ''Env

instance Default Env where def = Env [] def M.empty TM.empty True

-- | This type represents the configuration of the server. It stays constant for the whole run time of the server, and does not
-- change between requests. 
data Config = Config
  { _ghcSession        :: !(IORef HscTypes.HscEnv)
  , _errors            :: !(MVar (DL.DList GHCError))
  , _setupConfigDirty  :: !(IORef Bool)        
  , _packageDBDirty    :: !(IORef Bool)  
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

instance MonadIO (GhcServerM t) => MonadUtils.MonadIO (GhcServerM t) where
  liftIO = Control.Monad.Reader.liftIO

instance (MonadTrans (t Env), MonadBaseControl IO (ServerCoreM t)) => Exception.ExceptionMonad (GhcServerM t) where
  gcatch = catch
  gmask f = mask $ \g -> f g -- Cannot be eta-reduced because of higher rank types

instance GhcMonad.GhcMonad (GhcServerM t) => DynFlags.HasDynFlags (GhcServerM t) where
  getDynFlags = GhcMonad.getSessionDynFlags

instance (MonadTrans (t Env), MonadBaseControl IO (ServerCoreM t), MonadIO (ServerCoreM t)) => GhcMonad.GhcMonad (GhcServerM t) where
  getSession = readVar ghcSession
  setSession = writeVar ghcSession
                                              
-- | Execute an action on the current environment in the GhcServerM monad.
withEnv :: MFunctor (t Env) => t Env Identity a -> GhcServerM t a
withEnv = GhcServerM . hoist (return . runIdentity)

-- | View the part of the config viewed by the given Getting in the GhcServerM monad.
viewConfig :: MonadTrans (t Env) => Getting a Config a -> GhcServerM t a
viewConfig = GhcServerM . lift . view

-- | Read an IORef contained in the config.
readVar :: (Monad (ServerCoreM t), MonadTrans (t Env)) => Getting (IORef a) Config (IORef a) -> GhcServerM t a
readVar = viewConfig >=> liftIO . readIORef

-- | Write an IORef contained in the config.
writeVar :: (Monad (ServerCoreM t), MonadTrans (t Env)) => Getting (IORef a) Config (IORef a) -> a -> GhcServerM t ()
writeVar l v = viewConfig l >>= liftIO . flip writeIORef v

-- | The Handler monad is used for the functions that implement commands (like compile / info / ...). It has no mutable state,
-- the environment is frozen. It also keeps a reference to the global server configuration.
type Handler = GhcServerM ReaderT
-- | The Server monad is used by the server itself. The environment is mutable, whereas the server configuration is not.
type Server = GhcServerM StateT

-- | Run a Handler in the server monad, by taking the current environment and freezing it.
runHandler :: Handler a -> Server a
runHandler (GhcServerM h) = do
  s <- withEnv get
  GhcServerM $ lift $ runReaderT h s

-- | Run the server monad. This takes care of running the ghc monad. It does not set exceptions handlers, however.
runServer :: Server a -- ^ The action to run
          -> IO a
runServer s = do
  errs <- newEmptyMVar
  ses <- newIORef (Panic.panic "empty session")
  setupConfDirty <- newIORef True
  pkgDBDirty   <- newIORef False

  let stopWatching = do
        managers <- withEnv $ uses packageDBWatchers M.elems
        liftIO $ mapM_ stopManager managers >> putStrLn "Shutdown of Server monad finished."

  res <- withWatchCabal setupConfDirty $ flip runReaderT (Config ses errs setupConfDirty pkgDBDirty) $ flip evalStateT def $ unGhcServerM $ flip finally stopWatching $ do
    GHC.initGhcMonad $ Just GHC.Paths.libdir
    dflags <- GHC.getSessionDynFlags >>= GHC.setSessionDynFlags >> GHC.getSessionDynFlags -- Init the session
    mapM_ watchPackageDB [GlobalDB, UserDB]
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

      unless (M.member db mans) $ do
        man <- liftIO startManager
        withEnv $ packageDBWatchers . at db ?= man
        dir <- liftIO $ doesDirectoryExist path'

        let path''
              | dir = path'
              | otherwise = takeDirectory path'
            predicate ev
              | dir = True
              | otherwise = P.decodeString path' == eventPath ev
        void $ liftIO $ watchDir man (P.decodeString path'') predicate $ const $ writeIORef packageDBChanged True

-- | Stop watching a package db. After calling this function, the packageDBDirty IORef won't be 
-- set anymore when the DB is changed.
unwatchPackageDB :: PackageDB -> Server ()
unwatchPackageDB db = do
  man <- withEnv $ use $ packageDBWatchers . at db
  case man of
    Nothing -> return ()
    Just m -> do
      liftIO $ stopManager m
      withEnv $ packageDBWatchers . at db .= Nothing

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
withWatchCabal :: IORef Bool -> IO a -> IO a
withWatchCabal changed action = do
  wd <- fmap P.decodeString getCurrentDirectory
  withManager $ \distMan -> do
    void $ watchTree distMan wd (distPredicate wd) $ const $ writeIORef changed True
    action
  where distPredicate wd e = (wd P.</> "dist/setup-config") == eventPath e
  
