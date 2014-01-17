{-# LANGUAGE OverloadedStrings #-}
module Server.Compile
  ( compile
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.Monoid
import qualified Data.Text as T
import qualified Exception
import qualified GHC
import           Message
import qualified Panic
import           Pipes
import           Server.Configure
import           Server.Handler

compile :: FilePath -> Producer Message Handler Result
compile file = do
  
  let s = status "compile"
  s 1 $ "Compiling " <> T.pack file <> " ..."
  
  s 2 "Reset targets"
  void $ lift $ GHC.setTargets [] >> GHC.load GHC.LoadAllTargets
  
  s 2 "Compile target"
  file' <- lift $ resolveFile file
  flagE <- withFileOptions file' $ do
    GHC.guessTarget file' Nothing >>= GHC.setTargets . (:[])
    let handler err = GHC.printException err *> return GHC.Failed
    Exception.gtry $ GHC.handleSourceError handler (GHC.load GHC.LoadAllTargets)

  case flagE of
    Left e -> Failure 1 <$ yield (CompilerException $ T.pack $ show (e :: Panic.GhcException))
    Right GHC.Succeeded -> return Success
    Right GHC.Failed    -> return $ Failure 1
