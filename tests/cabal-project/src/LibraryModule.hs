{-# LANGUAGE CPP #-}
module LibraryModule where

import Data.SomeModule

-- We have at least base 4.4, per cabal file. This is just to test 
-- if we properly include the autogen file.
#if MIN_VERSION_base(4,4,0)
test :: ()
test = test2
#endif
