module Util
  ( takeWhileRight
  , eitherP
  ) where

import Control.Monad
import Pipes
import Pipes.Core

-- $setup
-- >>> import Pipes.Prelude (toList) 
-- >>> import qualified Pipes.Prelude as P

-- | @takeWhileRight@ passes values in a Right downstream, until it reaches a Left. The value in the Left is returned.
--
-- >>> toList $ each [Right 3, Right 4, Right 5, Left 4, Right 10] >-> (takeWhileRight >-> P.map Right >>= yield . Left)
-- [Right 3,Right 4,Right 5,Left 4]
takeWhileRight :: Monad m => Pipe (Either r a) a m r
takeWhileRight = await >>= either return (yield >=> const takeWhileRight)


-- | @eitherP f g@ is a consumer that runs p when it encounters a Left, and g if it encounters a Right.
--
-- >>> toList $ each [Right "hello", Left 3, Left 5, Right "bye", Left 6] >-> eitherP (\a -> yield a >-> P.map show) (\b -> yield b >-> P.map (show . length))
-- ["5","3","5","3","6"]
eitherP :: Monad m => (a -> Server x' x m ()) -> (b -> Server x' x m ()) -> Proxy () (Either a b) x' x m r
eitherP f g = do
  e <- await
  case e of
    Left a -> closed >\\ f a
    Right b -> closed >\\ g b
  eitherP f g
