{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternGuards #-}
module Daemon
  ( startServer
  , sendCommand
  , withUnixS
  , forkUnixS
  , Server
  ) where

import           Control.Applicative
import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad
import           Control.Monad.Extra hiding (bind)
import           Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Monoid
import qualified GHC
import           Message
import           Network.Socket hiding (send, recv)
import qualified Network.Socket.ByteString.Lazy as NB
import           Pipes
import           Pipes.Concurrent
import           Pipes.Network.TCP (fromSocket, toSocket)
import qualified Pipes.Prelude as P
import           System.Directory
import           System.Exit
import           System.IO
import           System.IO.Error
import           System.Posix

-- | A server is just a function that takes a producer of requests and writes it's responses to a
-- given producer.
type Server i o = Producer i IO () -> Consumer o IO () -> IO ()

-- | @startServer f s@ listens on the given Socket and starts the server. The socket must be
-- bound to an address and in listening state.
startServer :: (FromJSON a, ToJSON r, ToJSON m) => Server a (Either r m) -> Socket -> IO ()
startServer f s = do
  (reqOut,reqIn) <- spawn Unbounded
  (resOut,resIn) <- spawn Unbounded

  a <- async $ forever $ do
    putStrLn "Waiting for clients ..."
    bracket (fmap fst $ accept s) (close >=> const (putStrLn "Closed client connection.")) $ \c -> do
      putStrLn "Accepted client."
      void $ waitAnyCancel <=< mapM async $ 
       [ do runEffect $ fromSocket c 4096 >-> sepP 0 >-> P.filter (not . BS.null) >-> jsonP onError >-> toOutput reqOut
            putStrLn "Connection terminated by the client. "
       , do runEffect $ fromInput resIn >-> handleEither >-> jsonS >-> sepS 0 >-> toSocket c
            putStrLn "Connection terminated by the server."                   
       ]

  f (fromInput reqIn) (toOutput resOut) `finally` cancel a
  putStrLn "Performing garbage collection to exit ..."
  performGC
  putStrLn "Server handler exited. Stopping accept thread ..."
  cancel a

  where onError x = putStrLn x >> return False
        handleEither = await >>= either (yield . Left) (yield . Right >=> const handleEither)

-- | Close a unix socket. This will also unlink the socket file.
closeUnixSocket :: FilePath -> Socket -> IO ()
closeUnixSocket p s = putStrLn "Closing socket" >> close s >> removeFile p `catch` \(_ :: IOException) -> return ()

-- | @withUnixS p f c@ tries to connect to a unix socket bound to the file @p@. If that fails, it calls f and tries
-- again after f returned. If a connection can be established, the connected socket is passed to c and the result of
-- c is returned in a Just. If connecting fails, Nothing is returned.
withUnixS :: FilePath -> (FilePath -> IO ()) -> (Socket -> IO r) -> IO (Maybe r)
withUnixS p f c =
  bracket (socket AF_UNIX Stream 0) close $ \s -> do
    catch (connect s $ SockAddrUnix p) $ \(_ :: IOException) -> do
      f p
      catch (connect s $ SockAddrUnix p) $ \(_ :: IOException) -> return ()
    connected <- isConnected s
    if connected
      then Just <$> c s
      else return Nothing

-- | @forkUnixS f s p@ runs the server s in a new daemon process. It listens on the unix socket
-- at the path @p@. @f@ is the path to the log file.
-- When this function returns, connections are accepted on the given unix socket.
forkUnixS :: (FromJSON i, ToJSON r, ToJSON m) => FilePath -> Server i (Either r m) -> FilePath -> IO ()
forkUnixS f s p = do 
  -- Remove the file if it already exists, but only if it is a socket.
  -- The rationale behind this is that if the file is a socket, it was probably
  -- created by our program, so it's safe to delete it.
  om when (doesFileExist p) $ do
    sock <- fmap isSocket $ getFileStatus p
    unless sock $ ioError $ mkIOError alreadyExistsErrorType "ghc-server:forkUnixS" Nothing (Just p)
    removeFile p

  -- Create the socket. If we created the socket only in the forked server process, then it wouldn't
  -- be ready to accept connections right after the return of this function. That's the reason why
  -- the socket is already bound here.
  sock <- socket AF_UNIX Stream 0
  bind sock $ SockAddrUnix p
  listen sock maxListenQueue

  -- Daemonize
  void $ forkProcess $ child1 sock

  where child1 sock = do
          void createSession
          void $ forkProcess $ child2 sock
          exitSuccess

        child2 sock = do           
          void $ setFileCreationMask 0

          -- Remap STDIN/ERR/OUT to the log file (which might be /dev/null or a real log file).
          mapM_ closeFd [stdInput, stdOutput, stdError]
          fd <- openFd f ReadWrite (Just $ 8 ^ (3 :: Int) - 1) defaultFileFlags { trunc = True }
          mapM_ (dupTo fd) [stdInput, stdOutput, stdError]
          closeFd fd

          -- Stdout or stderr might be block buffered if bound to file. We use stdout/stderr for
          -- logging purposes, so there should be no buffering.
          hSetBuffering stdout NoBuffering 
          hSetBuffering stderr NoBuffering 
          putStrLn "Server process setup done."         

          -- Close the socket if we get killed
          forM_ [sigHUP, sigINT, sigTERM, sigPIPE] $ \x -> 
            installHandler x (Catch $ putStrLn "Caught signal. Exiting." >> closeUnixSocket p sock >> exitImmediately ExitSuccess) Nothing
          putStrLn "Signal handlers set."

          -- Now start the server process
          ei <- try $ flip finally (closeUnixSocket p sock) $ do  
            startServer s sock
            putStrLn "Server exited."
 
          case ei of
            Left e
              | Just (GHC.Signal _) <- fromException e -> putStrLn "Exiting because of signal" >> exitSuccess -- GHC API changes signal handlers, WTF ?!
              | otherwise                             -> putStrLn "Exception:" >> print e     >> exitFailure 
            Right () -> putStrLn "Exit success." >> exitImmediately ExitSuccess
 
-- | Send a command to the server. The second argument is the consumer which processes the responses of the server. The socket
-- must be connected to the server.
sendCommand :: (FromJSON i, ToJSON i, FromJSON o, ToJSON o) => (String -> IO Bool) -> i -> Consumer o IO r -> Socket -> IO (Maybe r)
sendCommand jsonError req c s = do
  void $ NB.send s $ encode req <> LBS.singleton 0
  runEffect $ (Nothing <$ fromSocket s 20 >-> sepP 0 >-> jsonP jsonError) >-> fmap Just c

