{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Check where

import           Build_ghc_server_tests
import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.List
import           Data.Monoid
import qualified Data.Text as T
import           System.Directory
import           System.Exit
import           System.FilePath
import           Test.Tasty
import           Test.Tasty.HClTest

packagedb :: String
#if __GLASGOW_HASKELL__ >= 706
packagedb = "package-db"
#else
packagedb = "package-conf"
#endif

ghcserver :: FilePath
ghcserver = getDistDir </> "build/ghc-server/ghc-server"

testFailure :: Maybe FilePath -> Int -> [String] -> HClTest Trace ()
testFailure wd c a = withLog $ testExitCode wd 1000 ghcserver (defaultOpts ++ a) $ ExitFailure c
  where defaultOpts = ["-v","3","-f","log","check"]

testSuccess :: Maybe FilePath -> [String] -> HClTest Trace ()
testSuccess wd a = withLog $ testExitCode wd 1000 ghcserver (defaultOpts ++ a) ExitSuccess
  where defaultOpts = ["-v","3","-f", "log","check"]

copySources :: FilePath -> HClTest w ()
copySources x = liftIO $ copyFilesHere (head getSrcDirs </> x)

startServer :: HClTest Trace ()
startServer = testExitCode Nothing 1000 ghcserver ["-v", "3","-f","log", "admin", "start"] ExitSuccess

withLog :: HClTest Trace () -> HClTest Trace ()
withLog x = x <|> showLog
  where  showLog :: HClTest Trace ()
         showLog = testStep "Log: " $ liftIO (readFile "log") >>= traceMsg >> mzero

testSampleError :: HClTest Trace ()
testSampleError = do
  wd <- liftIO getCurrentDirectory
  testStdout Nothing 1000 ghcserver ["-v","3","check","SampleError.hs"] (ExitFailure 1) $
    T.pack (wd </> "SampleError.hs") <> ":9:5: Not in scope: `foo'" <> "\n"

partitionM   :: Monad m => (a -> m Bool) -> [a] -> m ([a],[a])
partitionM f = flip foldM ([],[]) $ \(a,b) x -> do
  v <- f x
  return $ if v then (x:a,b) else (a,x:b)

findHaskellFiles :: FilePath -> IO [FilePath]
findHaskellFiles dir = do
  (files, dirs) <- partitionM (doesFileExist . (dir </>)) . filter (not . flip elem [".",".."]) =<< getDirectoryContents dir
  files' <- fmap concat $ forM dirs $ findHaskellFiles . (dir </>)
  return $
       map (dir </>) (filter (liftM2 (||) (".hs" `isSuffixOf`) (".hsc" `isSuffixOf`)) files)
    ++ files'

tests :: TestTree
tests = testGroup "check" 
  [ hcltest "without cabal" $ do 
      copySources "without-cabal"
      randomParallel 3 $ concat $ replicate 5
        [ testSuccess Nothing ["Sample1.hs"]
        , testSuccess Nothing ["Parent.hs"]
        , testSuccess Nothing ["Child.hs"]
        , testSampleError
        ]

  , hcltest "package db reloading" $ do
      copySources "db-reloading"
      startServer
      testExitCode Nothing 1000 "ghc-pkg" ["init", "pkgdb"] ExitSuccess
      testExitCode Nothing 1000 ghcserver ["-v", "3", "admin", "ghc", packagedb ++ " pkgdb"] ExitSuccess
      testFailure Nothing 1 ["Main.hs"]
      -- WORKAROUND: There is a crazy bug in cabal 1.16 where installing directly (without going through unpack first)
      -- causes ghc-pkg to fail with a file not found error.
      testExitCode Nothing 1000 "cabal" ["unpack", "acme-dont-1.1"] ExitSuccess
      testExitCode (Just "acme-dont-1.1") 1000 "cabal" ["install", "--package-db=clear", "--package-db=global", "--package-db=../pkgdb"] ExitSuccess
      testExitCode Nothing 1000 ghcserver ["-v", "3", "admin", "status"] ExitSuccess
      testSuccess Nothing ["Main.hs"]

  , hcltest "cabal project is server root" $ do
      copySources "cabal-project"
      startServer
      forM_ ["src", "independent-src", "library-tests"] $ \dir ->  
        testExitCode (Just dir) 1000 ghcserver ["-v", "3", "admin", "status"] ExitSuccess

  , hcltest "cabal project support" $ do
      copySources "cabal-project"
      startServer
      testFailure Nothing 1 ["src/LibraryModule.hs"]
      -- Test cabal reloading
      testExitCode Nothing 1000 "cabal" ["configure", "--enable-tests", "--enable-benchmarks"] ExitSuccess
      -- Test that the project is actually buildable, and also preprocess the sources (and generate autogen files)
      testExitCode Nothing 1000 "cabal" ["build"] ExitSuccess
      randomParallel 3 $ concat $ replicate 5
        [ testSuccess (Just "library-tests")  ["Main.hs"]
        , testSuccess (Just "independent-src") ["Main.hs"]
        , testSuccess (Just "src") ["LibraryModule.hs"]
        , testSuccess (Just "src") ["Data/SomeModule.hs"]
        , testSuccess Nothing ["library-tests/Main.hs"]
        , testSuccess Nothing ["src/LibraryModule.hs"]
        ] 

  , hcltest "self" $ do
      -- Copy ourselves, so we don't accidently mess up the project!
      sources <- liftIO $ do
        forM_ ["src", "dist", "tests", ".cabal-sandbox"] $ \x -> do
          let source = head getSrcDirs </> ".." </> x
          e <- doesDirectoryExist source
          when e $ createDirectory x >> copyFiles source x
        mapM_ (flip copyFile "." . (head getSrcDirs </>)) <=< filterM doesFileExist $ ["../ghc-server.cabal", "../Setup.hs", "../cabal.sandbox.config"] 
        findHaskellFiles "src" 

      let sources' = sources
                  ++ [ "tests/Admin.hs"
                     , "tests/Check.hs"
                     , "tests/Main.hs"
--                     , "tests/doctests.hsc" -- We cannot handle .hsc yet
                     ]
      randomParallel 3 $ concat $ replicate 5 $ map (testSuccess Nothing . (:[])) sources'
  ]


