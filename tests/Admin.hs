module Admin (tests) where

import Build_ghc_server_tests
import Control.Monad
import System.Directory
import System.Exit
import System.FilePath
import System.Posix.Files
import Test.Tasty
import Test.Tasty.HClTest

testFailure :: Int -> [String] -> HClTest Trace ()
testFailure c a = testExitCode Nothing 1000 (getDistDir </> "build/ghc-server/ghc-server") (defaultOpts ++ a) $ ExitFailure c
  where defaultOpts = ["-v","3","admin"]

testSuccess :: [String] -> HClTest Trace ()
testSuccess a = testExitCode Nothing 1000 (getDistDir </> "build/ghc-server/ghc-server") (defaultOpts ++ a) ExitSuccess
  where defaultOpts = ["-v","3","admin"]

tests :: TestTree
tests = testGroup "admin"
  [ hcltest "start stop status" $ do
      testFailure 1 ["status"]
      testSuccess   ["start"]
      testSuccess   ["status"]
      testSuccess   ["stop"]
      testFailure 1 ["status"]

  , hcltest "start stop socket file" $ replicateM_ 25 $ do -- There was a bug that only appeared very infrequent, so run these tests 25 times to cover that in future.
      testSuccess ["start"]
      testIO "socket file does exist" $ fmap isSocket $ getFileStatus ".ghc-server.sock"
      testSuccess ["stop"]
      testIO "socket file doesn't exist anymore" $ fmap not $ doesFileExist ".ghc-server.sock"

  , hcltest "double start" $ do
      testSuccess ["start"]
      testSuccess ["start"]
      testSuccess ["status"]
      testSuccess ["stop"]

  , hcltest "double stop" $ do
      testSuccess ["stop"]
      testSuccess ["start"]
      testSuccess ["stop"]
      testSuccess ["stop"]
      testFailure 1 ["status"]
  ] 
