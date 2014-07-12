{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE FlexibleInstances  #-}
import           Control.Concurrent (threadDelay)
import qualified Control.Exception as E
import           Control.Lens hiding (argument)
import           Control.Monad
import           Control.Monad.Loops
import           Control.Monad.State
import           Daemon
import           Data.Default
import           Data.List
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Message
import           Options.Applicative hiding ((&), ParserResult(..))
import           Pipes
import           Server.Main
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.IO
import           Util

-- | Options that are shared by all subcommands
data SharedOptions = SharedOptions
  { _verbosity    :: Integer           -- ^ The verbosity level. Applies to client and server
  , _neverStart   :: Bool              -- ^ If True, do not start the server when it's not running
  , _disableWarnings :: Bool           -- ^ Do not print compiler warning
  , _logFile         :: Maybe FilePath -- ^ Log file for the server
  , _disableCabal    :: Bool           -- ^ Disable cabal support
  , _socketFile      :: FilePath       -- ^ The socket file to use
  , _serverTimeout   :: Maybe Int      -- ^ Number of seconds after which the server will suicide. 0 means no timeout.
  } deriving Show
makeLenses ''SharedOptions

sharedOptions :: Parser SharedOptions
sharedOptions = SharedOptions
  <$> option (short 'v' <> long "verbose" <> help "Verbosity level" <> metavar "LEVEL" <> value 0)
  <*> switch (short 'r' <> long "disable-start" <> help "Do not start the server if it's not running")
  <*> switch (short 'w' <> long "no-warnings" <> help "Do not print compiler warnings")
  <*> optional (strOption (short 'f' <> long "log" <> help "logfile" <> metavar "FILE"))
  <*> switch (short 'c' <> long "no-cabal" <> help "Do not use cabal to figure out the project settings")
  <*> option (short 's' <> long "socket" <> help "The path to the socket file to use. Relative paths are relative to the project root if cabal support is enabled." <> value ".ghc-server.sock")
  <*> optional (option (short 't' <> long "timeout" <> help "Number of seconds of idle time after which the server will exit. If set to 0, the server will never exit by itself."))

-- | Configure options for the client's pipeline.
data Config = Config
  { -- | Function to start the server in case it's not already running
    _serverStarter  :: Server Request Response -> FilePath -> IO ()
  , _onSuccess      :: IO ()     -- ^ Action to be executed if the server returned Success
  , _onFailure      :: IO ()     -- ^ Action to be executed if the server returned Failure
  , _onStartFailure :: IO ()     -- ^ Action to be executed if the starting the server failed
  , _request        :: Request  -- ^ The request which is sent to the server
  , _searchCabal    :: Bool     -- ^ Search for a cabal configuration file and change the working directory to
                               -- that project root?
  }
makeLenses ''Config

newtype ConfigO = ConfigO (SharedOptions -> Config)
makeIso ''ConfigO

instance Default ConfigO where
  def = ConfigO d
    where d options = Config starter (return ()) (return ()) startFailure (Multiple []) True
            where starter process p = unless (options^.neverStart) $ do
                    logClient options 1 "Starting server"
                    forkUnixS (fromMaybe "/dev/null" $ options^.logFile) process p

                  startFailure = logClient options 0 "Failed to start server."

logClient :: SharedOptions -> Integer -> T.Text -> IO ()
logClient o i m = when (o^.verbosity >= i) $ T.hPutStrLn stderr $ "[Client] " <> m

requestConfig :: Request -> ConfigO
requestConfig r = def & from configO.mapped.request .~ r

commandConfig :: String -> ConfigO -> InfoMod ConfigO -> Mod CommandFields ConfigO
commandConfig c = commandConfigM c . pure

commandConfigM :: String -> Parser ConfigO -> InfoMod ConfigO -> Mod CommandFields ConfigO
commandConfigM c conf = command c . info conf

customConfig :: (SharedOptions -> State Config ()) -> ConfigO
customConfig f = ConfigO $ \opts -> execState (f opts) (review configO def opts)

adminCmd :: ParserInfo ConfigO
adminCmd = info (helper <*> cmd) $ fullDesc <> progDesc "command the server"
  where cmd = subparser $ mconcat
           [ commandConfigM "start"  startParser     $ briefDesc <> progDesc "start the server"
           , commandConfig  "stop"   shutdownConfig  $ briefDesc <> progDesc "stop the server"
           , commandConfig  "reset"  resetConfig     $ briefDesc <> progDesc "reset the GHC options used by the server and reload the cabal file"
           , commandConfig  "status" statusConfig    $ briefDesc <> progDesc "check whether the server is running or not"
           , commandConfigM "ghc"    ghcParser       $ fullDesc  <> progDesc "add GHC options for compiling"
           ]

        startParser = startConfig <$> switch (short 'd' <> long "no-daemon" <> help "Do not daemonize")

        startConfig nd = customConfig $ \opts -> do
          when nd $ serverStarter .= \s p ->
            E.bracket (bindUnixSocket p) (maybe (return ()) $ closeUnixSocket p) $ \sock -> case sock of
              Nothing    -> logClient opts 0 "Race condition detected. Not starting server."
              Just sock' -> startServer s sock'

          onSuccess     .= logClient opts 0 "Server is running now."
          onFailure     .= logClient opts 0 "Failed to start server."

        shutdownConfig = customConfig $ \opts -> do
          serverStarter .= \_ _ -> logClient opts 0 "Server is not running." >> exitSuccess
          onSuccess     .= do
            whileM_ (doesFileExist $ opts^.socketFile) $ threadDelay 100000
            logClient opts 0 "Server stopped."
          onFailure     .= logClient opts 0 "Failed to stop server."
          request       .= Shutdown

        resetConfig = customConfig $ const $ request .= EnvChange ResetGhcArgs

        statusConfig = customConfig $ \opts -> do
          serverStarter .= \_ _ -> logClient opts 0 "Server is not running." >> exitFailure
          onSuccess     .= logClient opts 0 "Server is running."

        ghcParser = requestConfig . EnvChange . AddGhcArgs . map ('-':)
                 <$> many (argument Just (metavar "FLAG" <> help "a GHC flag, without the leading dash"))

checkCmd :: FilePath -> ParserInfo ConfigO
checkCmd pwd = info (helper <*> cmd) $ fullDesc <> progDesc "check a file for errors and warnings"
  where cmd = requestConfig . Command pwd . Check <$> argument Just (metavar "FILE" <> help "The file which to check for errors")

infoCmd :: FilePath -> ParserInfo ConfigO
infoCmd pwd = info (helper <*> cmd) $ fullDesc <> progDesc "print information about an identifier"
  where cmd = fmap (requestConfig . Command pwd) $ Info
                <$> argument Just (metavar "FILE" <> help "The file of the indentifier")
                <*> argument (Just . T.pack) (metavar "IDENTIFIER" <> help "The identifier to retrieve information for")

typeCmd :: FilePath -> ParserInfo ConfigO
typeCmd _ = info (helper <*> cmd) $ fullDesc <> progDesc "get the type of an expression at the given place"
  where cmd = undefined

cmds :: FilePath -> Parser (SharedOptions, ConfigO)
cmds pwd = (,) <$> sharedOptions <*> individual
  where individual = subparser $ mconcat
          [ command "check" $ checkCmd pwd
          , command "info"  $ infoCmd pwd
          , command "type"  $ typeCmd pwd
          , command "admin"   adminCmd
          ]

renderMessage :: SharedOptions -> Message -> IO ()
renderMessage o (Log l v t) = when (o^.verbosity >= v) $ T.hPutStrLn stderr $ "[Server:" <> l <> "] " <> t
renderMessage _ (CompilerError t) = T.putStrLn t
renderMessage o (CompilerWarning t) = unless (o^.disableWarnings) $ T.putStrLn t
renderMessage _ (CompilerException t) = T.putStrLn $ "[Server:GHC Exception] " <> t
renderMessage _ (InternalError t) = T.putStrLn $ "[Server:Internal error] " <> t
renderMessage _ (UnimplementedError t) = T.putStrLn $ "[Server:Unimplemented] " <> t

findCabal :: FilePath -> IO (Maybe FilePath)
findCabal path = do
  parent <- canonicalizePath $ path </> ".."
  files <- filterM doesFileExist . map (path </>) =<< getDirectoryContents path
  case find (".cabal" `isSuffixOf`) files of
    Nothing -> if parent /= path then findCabal parent else return Nothing
    Just _  -> return $ Just path

main :: IO ()
main = do
  pwd <- getCurrentDirectory
  (options, req') <- execParser (opts pwd)
  let req = review configO req' options
      req'' | not $ options^.disableCabal = req^.request
            | otherwise = Multiple [EnvChange DisableCabal, req^.request]
      addTimeoutOpt (Just t) | t == 0 = Multiple [EnvChange $ SuicideTimeout Nothing, req'']
                             | otherwise = Multiple [EnvChange $ SuicideTimeout $ Just t, req'']
      addTimeoutOpt _ = req''
      req''' = addTimeoutOpt (options^.serverTimeout)

  unless (options^.disableCabal) $
    when (req^.searchCabal) $ findCabal pwd >>= maybe (return ()) setCurrentDirectory
  r <- withUnixS ".ghc-server.sock" (req^.serverStarter $ serve) $ sendCommand (onError options) req''' $ client options

  case r of
    Nothing -> req^.onStartFailure
    Just res -> case res of
      Nothing -> hPutStrLn stderr "[Error] Server connection lost before end" >> exitWith (ExitFailure $ -2)
      Just s -> case s of
        Success -> req^.onSuccess
        Failure code -> req^.onFailure >> exitWith (ExitFailure code)

  where opts pwd = info (helper <*> cmds pwd)
          ( fullDesc
         <> progDesc "A persistent background ghc process"
          )

        client :: SharedOptions -> Consumer Response IO Result
        client options = do
          lift $ logClient options 2 "Starting read loop"
          for takeWhileRight $ lift . renderMessage options

        onError :: SharedOptions -> String -> IO Bool
        onError options x = fmap (const True) $ logClient options 0 $ T.pack $ "Failed to parse server response: " <> x
