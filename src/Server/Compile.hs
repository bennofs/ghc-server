{-# LANGUAGE OverloadedStrings #-}
module Server.Compile
  ( compile
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.Monoid
import qualified Data.Text as T
import qualified GHC
import           Message
import           Pipes
import           Server.Configure
import           Server.Handler

compile :: FilePath -> Producer Message Handler Result
compile file = do
  
  let s = status "compile"
  s 1 $ "Compiling " <> T.pack file <> " ..."
  
  s 2 "Reset targets"
  void $ lift $ GHC.setTargets [] >> GHC.load GHC.LoadAllTargets
  
  s 2 "Load current target"
  file' <- lift $ resolveFile file
  loadFileOptions file'
  lift $ GHC.guessTarget file' Nothing >>= GHC.setTargets . (:[])
  
  s 2 "Compile target"
  let handler err = GHC.printException err *> return GHC.Failed
  flag <- lift $ GHC.handleSourceError handler $ GHC.load GHC.LoadAllTargets
  case flag of
    GHC.Succeeded -> return Success
    GHC.Failed    -> return $ Failure 1
