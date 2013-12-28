module Util
  ( takeWhileRight
  ) where

import Control.Monad
import Pipes

-- $setup
-- >>> import Pipes.Prelude (toList) 
-- >>> import qualified Pipes.Prelude as P

-- | @takeWhileRight@ passes values in a Right downstream, until it reaches a Left. The value in the Left is returned.
--
-- >>> toList $ each [Right 3, Right 4, Right 5, Left 4, Right 10] >-> (takeWhileRight >-> P.map Right >>= yield . Left)
-- [Right 3,Right 4,Right 5,Left 4]
takeWhileRight :: Monad m => Pipe (Either r a) a m r
takeWhileRight = await >>= either return (yield >=> const takeWhileRight)
