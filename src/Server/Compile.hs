{-# LANGUAGE OverloadedStrings #-}
module Server.Compile
  ( compile
  ) where

import           Control.Applicative
import           Control.Lens
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as T
import qualified Exception
import qualified GHC
import qualified GhcMonad
import qualified HscTypes
import qualified Linker
import           Message
import qualified Module
import qualified Panic
import           Pipes
import           Server.Configure
import           Server.Handler
import qualified UniqFM

-- | Traversal for the module summary of a target. Note that this only satisfies
-- the traversal laws if you change neither the filename nor the module name.
targetModSummary :: Applicative f => HscTypes.TargetId -> (HscTypes.ModSummary -> f HscTypes.ModSummary) -> HscTypes.ModuleGraph -> f HscTypes.ModuleGraph
targetModSummary (HscTypes.TargetModule modname) f = traverse go
  where go ms | Module.moduleName (HscTypes.ms_mod ms) == modname = f ms
              | otherwise = pure ms
targetModSummary (HscTypes.TargetFile filename _) f = traverse go
  where go ms | maybe False (== filename) $ Module.ml_hs_file $ HscTypes.ms_location ms = f ms
              | otherwise = pure ms

getModuleName :: GHC.GhcMonad m => HscTypes.TargetId -> m (Maybe Module.ModuleName)
getModuleName t = preview (to HscTypes.hsc_mod_graph . targetModSummary t . to HscTypes.ms_mod . to Module.moduleName) <$> GhcMonad.getSession

unloadTarget :: (MonadIO m, GHC.GhcMonad m) => HscTypes.TargetId -> m ()
unloadTarget targetId = do 
  h <- GhcMonad.getSession
  mmodname <- getModuleName targetId
  case mmodname of
    Nothing -> return ()
    Just modname -> do
      let hpt = HscTypes.hsc_HPT h
          hpt' = UniqFM.delFromUFM hpt modname
      Exception.gmask $ \_ -> do 
        dflags <- GHC.getSessionDynFlags
        liftIO $ Linker.unload dflags $ mapMaybe HscTypes.hm_linkable $ UniqFM.eltsUFM hpt'
        GhcMonad.setSession $! h { HscTypes.hsc_HPT = hpt' }

compile :: FilePath -> Producer Message Handler Result
compile file = do
  
  let s = status "compile"
  s 1 $ "Compiling " <> T.pack file <> " ..."
  
  s 2 "Compile target"
  file' <- lift $ resolveFile file
  flagE <- withFileOptions file' $ do
    target <- GHC.guessTarget file' Nothing
    GHC.setTargets [target]    
    unloadTarget $ HscTypes.targetId target
    let handler err = GHC.printException err *> return GHC.Failed
    Exception.gtry $ GHC.handleSourceError handler (GHC.load GHC.LoadAllTargets)

  case flagE of
    Left e -> Failure 1 <$ yield (CompilerException $ T.pack $ show (e :: Panic.GhcException))
    Right GHC.Succeeded -> return Success
    Right GHC.Failed    -> return $ Failure 1
