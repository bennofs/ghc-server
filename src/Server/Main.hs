{-# LANGUAGE OverloadedStrings #-}
module Server.Main
  ( serve
  ) where

import           Control.Applicative
import           Control.Concurrent.MVar
import           Control.Lens
import           Control.Monad
import           Daemon (ControlMessage(..))
import qualified Data.DList as DL
import qualified Data.Text as T
import           Data.Void
import           Message
import           Pipes
import           Pipes.Core ((//>))
import qualified Pipes.Prelude as P
import           Server.Compile
import           Server.Configure
import           Server.Errors
import           Server.Handler
import           System.Directory
import           System.Exit
import           Util

-- | Start the request-response loop. Requests are read from the input producer and responses are written to the output
-- consumer. The process is consumer-driven, the consumer should start to request responses. 
serve :: Producer Request IO () -> Consumer Response IO () -> Consumer ControlMessage IO () -> IO ()
serve inp outp ctrl = runServer $ do
    runHandler initFlags
    runEffect $ for (hoist liftIO inp) (\req -> 
                      handleRequest req 
                  >-> eitherP (\x -> yield x >-> (hoist liftIO ctrl //> absurd)) (yield . Right . msg)
                  >>= yield . bimap finish finish)
            >-> (takeWhileRight >>= yield) 
            >-> hoist liftIO outp
  
-- | Handle a single request.
handleRequest :: Request -> Producer (Either ControlMessage Message) Server (Either Result Result)
handleRequest Shutdown  = return $ Left Success
handleRequest (Multiple rs)
  | null rs = return $ Right Success
  | otherwise = fmap last . sequence <$> mapM handleRequest rs
handleRequest (EnvChange e) = Right <$> handleEnvChange e
handleRequest (Command pwd c) = (>-> P.map Right) $ withErrors $ do
  status "handleRequest" 1 "Processing command ..."
  lift $ withEnv $ workingDirectory .= pwd

  rld <- lift $ viewConfig packageDBDirty >>= liftIO . tryTakeMVar
  when (has _Just rld) reloadPkgDBs

  cab <- lift $ viewConfig setupConfigDirty >>= liftIO . tryTakeMVar
  cabEnabled <- lift $ withEnv $ use cabalEnabled
  setupConfExists <- liftIO $ doesFileExist "dist/setup-config"
  when (has _Just cab && cabEnabled && setupConfExists) loadCabal

  fmap Right $ hoist runHandler $ handleCommand c

-- | Handle an EnvChange request.
handleEnvChange :: EnvChange -> Producer (Either ControlMessage Message) Server Result
handleEnvChange (AddGhcArgs a) = (>-> P.map Right) $ withErrors $ do
  status "handleEnvChange" 1 $ T.pack $ "Adding flags: " ++ unwords a
  lift $ withEnv $ compilerFlags <>= a
  status "handleEnvChange" 2 "Updating GHC flags ..."
  r <- lift $ addFlags a
  case r of
    Just err -> Failure 1 <$ yield (CompilerException $ T.pack err)
    _ -> return Success
handleEnvChange (SuicideTimeout t) = Success <$ yield (Left $ SetTimeout t)
handleEnvChange ResetGhcArgs = (>-> P.map Right) $ withErrors $ Success <$ resetFlags
handleEnvChange DisableCabal = (>-> P.map Right) $ withErrors $ Success <$ lift (withEnv $ cabalEnabled .= False)

-- | Send all collected errors.
sendErrors :: Producer Message Server ()
sendErrors = do
  status "sendErrors" 1 "Collecting error messages ..."
  msgs <- do
    pwd <- lift $ withEnv $ use workingDirectory
    errs <- lift $ viewConfig errors >>= liftIO . tryTakeMVar
    case errs of
      Nothing -> do
        yield $ InternalError "Error message weren't initialized, failed to take MVar"
        liftIO exitFailure
      Just errs' -> return $ map (errorToMessage . makeRelativeLocation pwd) $ DL.toList errs'
  mapM_ yield msgs

-- | Init the error message queue.
initErrors :: Producer Message Server ()
initErrors = do
  errs <- lift $ viewConfig errors
  success <- liftIO $ tryPutMVar errs DL.empty
  unless success $ do
      yield $ InternalError "Error messages weren't collected, MVar is still full."
      liftIO exitFailure

-- | Run an action that might produce errors. This will initialize the 
-- error collecting system before running the action and also collect and send 
-- the errors afterwards.
withErrors :: Producer Message Server a -> Producer Message Server a
withErrors a = initErrors *> a <* sendErrors

-- | Process a command.
handleCommand :: Command -> Producer Message Handler Result
handleCommand (Check f) = compile f
handleCommand (Info _ _) = Failure 2 <$ yield (UnimplementedError "Info command")
handleCommand (Type{}) = Failure 2 <$ yield (UnimplementedError "Type command")
