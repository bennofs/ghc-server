{-# LANGUAGE TemplateHaskell #-}
module Message
  ( Message(..)
  , Response, status, finish, msg          
  , Result(..)
  , Request(..)
  , Command(..)
  , EnvChange(..)
  , jsonP, jsonS, sepP, sepS
  ) where

import           Control.Monad
import           Control.Monad.State
import           Data.Aeson (eitherDecode', encode, FromJSON, ToJSON)
import           Data.Aeson.TH
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Monoid
import qualified Data.Text as T
import           Data.Word
import           Pipes
import           Pipes.Lift
import qualified Pipes.Prelude as P

-- | The verbosity level of a log message. Higher levels mean more verbosity. The smallest log level
-- is 0.
type LogLevel = Integer

-- | A message sent to the client.
data Message = Log T.Text LogLevel T.Text
             | CompilerError T.Text
             | CompilerWarning T.Text
             | CompilerException T.Text
             | InternalError T.Text
             | UnimplementedError T.Text
  deriving Show
deriveJSON defaultOptions ''Message

-- | The result of processing a request.
data Result = Success
            | Failure Int
  deriving Show
deriveJSON defaultOptions ''Result

-- | Response types sent by the server to the client.
type Response = Either Result Message

-- | Construct a response to finish with the given result.
finish :: Result -> Response
finish = Left

-- | Construct a response with the given message.
msg :: Message -> Response
msg = Right

-- | Log the current status. The first argument should be some text describing the location or
-- current phase. The second argument is the actual status message.
status :: Monad m => T.Text -> Integer -> T.Text -> Producer Message m ()
status p l = yield . Log p l

-- | A command describes an action that should be perfomed by the server.
data Command = Check FilePath
             | Info FilePath T.Text
             | Type FilePath [T.Text] T.Text
  deriving Show
deriveJSON defaultOptions ''Command

-- | A change to the environment.
data EnvChange = AddGhcArgs [String]
               | ResetGhcArgs
               | DisableCabal
               | SuicideTimeout (Maybe Int)
  deriving Show
deriveJSON defaultOptions ''EnvChange

-- | A request made by the client and sent to the server.
data Request = Command FilePath Command -- ^ The first argument is the working directory.
             | EnvChange EnvChange      -- ^ Perform a change to the environment
             | Shutdown                 -- ^ Exit the server
             | Multiple [Request]       -- ^ Send multiple requests at once. If the list of requests is empty, the server should always
                                        -- respond with Success and do nothing. This allos to use Multiple [] as sort of a ping message.
  deriving Show
deriveJSON defaultOptions ''Request

-- | Parses a stream of ByteStrings using Aeson. When a parse error occurs, the function passed as the argument will be called with error message.
-- If that function returns False, the pipe terminates. Otherwise, it continues with the next input.
jsonP :: (FromJSON o) => (String -> IO Bool) -> Pipe BS.ByteString o IO ()
jsonP f = do
  -- Workaround: eitherDecodeStrict' is broken in latest aeson, so we use eitherDecode'
  c <- fmap (eitherDecode' . LBS.fromChunks . return) await >>= either (lift . f) (yield >=> const (return True))
  when c $ jsonP f

-- | Serializes a stream of requests to ByteStrings.
jsonS :: (ToJSON i, Monad m) => Pipe i BS.ByteString m ()
jsonS = P.map (BS.concat . LBS.toChunks . encode)

-- | A pipe that takes arbitrary chunks of data and produces continous bytestrings, splitting the input on the given separator.
-- The separator is not included in the result. If the input ends with the separator, an empty string is yielded as the last
-- result.
sepP :: Monad m => Word8 -> Pipe BS.ByteString BS.ByteString m ()
sepP s = execStateP (BS.pack []) go >>= yield
  where go = do
          p <- get
          x <- await
          let bss = BS.splitWith (==s) $ p <> x
          unless (null bss) $ do
            each $ init bss
            put $ last bss
          go

-- | A pipe that appends the separator to each incoming ByteString. 
sepS :: Monad m => Word8 -> Pipe BS.ByteString BS.ByteString m ()
sepS s = P.map (<> BS.singleton s)
