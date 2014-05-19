{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Pretty printing and formatting of GHC errors.
module Server.Errors
  ( GHCError(), location, message, style
  , showError
  , collectErrors
  , errorToMessage
  , makeRelativeLocation
  ) where

import           Control.Concurrent.MVar
import           Control.Lens
import           Control.Monad
import qualified Data.DList as DL
import           Data.Data.Lens
import qualified Data.Text as T
import qualified DynFlags
import qualified ErrUtils
import qualified FastString as FS
import qualified GHC
import           Message
import qualified Outputable
import qualified SrcLoc
import           System.Exit
import           System.FilePath
#if __GLASGOW_HASKELL__ >= 706
import qualified Pretty
#endif

-- | A data type holding all the information of a GHC error message.
data GHCError = GHCError
  { _severity       :: GHC.Severity
  , _location       :: GHC.SrcSpan
  , _style          :: Outputable.PprStyle
  , _message        :: Outputable.SDoc
  , _renderDoc      :: Outputable.SDoc -> Outputable.PprStyle -> String
  }
makeLenses ''GHCError

-- | Pretty print a GHC error as a human-readable string.
showError :: GHCError -> String
#if __GLASGOW_HASKELL__ >= 706
showError err = (err ^. renderDoc . flipped) (err ^. style) $ ErrUtils.mkLocMessage (err ^. severity) (err ^. location) (err ^. message)
#else
showError err = (err ^. renderDoc . flipped) (err ^. style) $ ErrUtils.mkLocMessage (err ^. location) (err ^. message)
#endif

-- | Change the location of the error message to be relative to the given directory.
makeRelativeLocation :: FilePath -> GHCError -> GHCError
makeRelativeLocation wd = over (location . biplate) transformRealSpan
  where transformRealSpan s = SrcLoc.mkRealSrcSpan (transformLoc $ SrcLoc.realSrcSpanStart s) (transformLoc $ SrcLoc.realSrcSpanEnd s)
          where transformLoc l = SrcLoc.mkRealSrcLoc (FS.mkFastString $ makeRelative wd $ FS.unpackFS $ SrcLoc.srcLocFile l) (SrcLoc.srcLocLine l) (SrcLoc.srcLocCol l)


-- | COnvert a GHCError to a Message to be sent to the client.
errorToMessage :: GHCError -> Message
errorToMessage e = case e^.severity of
  GHC.SevWarning -> CompilerWarning $ T.pack $ showError e
  GHC.SevError   -> CompilerError   $ T.pack $ showError e
  GHC.SevFatal   -> CompilerError   $ T.pack $ showError e
  _              -> Log "GHC" 0     $ T.pack $ showError e

assertFull :: MVar a -> IO ()
assertFull a = do
  e <- isEmptyMVar a
  when e $ do
    putStrLn "Error: Error messages weren't initialized"
    exitFailure

-- | A GHC LogAction that collects all the errors and writes them to the given output sink.
collectErrors :: MVar (DL.DList GHCError) -> DynFlags.LogAction
#if __GLASGOW_HASKELL__ >= 706
collectErrors out dflags sev sspan pprstyle m = assertFull out >> void (modifyMVar_ out $ return . (`DL.snoc` err))
  where err = GHCError sev sspan pprstyle m $ \doc -> showDoc . toDoc doc
        toDoc doc style' = Outputable.runSDoc doc (Outputable.initSDocContext dflags style')
        showDoc = Pretty.fullRender Pretty.PageMode (GHC.pprCols dflags) 1.5 stringTxt ""

stringTxt :: Pretty.TextDetails -> String -> String
stringTxt (Pretty.Chr c)   s  = c:s
stringTxt (Pretty.Str s1)  s2 = s1 ++ s2
stringTxt (Pretty.PStr s1) s2 = FS.unpackFS s1 ++ s2
stringTxt (Pretty.LStr s1 _) s2 = FS.unpackLitString s1 ++ s2
#else
collectErrors out sev sspan pprstyle m = assertFull out >> void (modifyMVar_ out $ return . (`DL.snoc` err))
  where err = GHCError sev sspan pprstyle m Outputable.renderWithStyle
#endif
