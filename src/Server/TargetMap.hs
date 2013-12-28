module Server.TargetMap
  ( TargetMap()
  , lookupBest
  , insert
  , empty
  , union
  , fromTargets
  , canonicalizeTarget
  ) where

import           Control.Applicative hiding (empty)
import           Data.List hiding (union, insert)
import qualified Data.Map as M
import           Data.Maybe
import           Data.Monoid
import           Distribution.Client.Dynamic
import           System.Directory
import           System.FilePath

-- | A target map maps source directories to targets which specify that directory or a parent in the
-- hs-source-dirs field.
data TargetMap = TargetMap !(Maybe [Target]) !(M.Map String TargetMap) deriving (Read, Show)

-- | Lookup the targets that have the given directory or a parent as it's source dir. If there are
-- multiple matches, it returns the best targets, which are the targets with the source directory that
-- is the closest to the given directory (i.e. the most specific one).
-- 
-- Example:
--
-- >>> :{
--  let
--    test = Target (TestSuite "test") [] ["tests"] [] [] [] False False
--    test2 = Target (TestSuite "test2") [] ["tests"] [] [] [] False False
--    specific = Target (TestSuite "specific") [] ["tests/specific"] [] [] [] False False
--    test3 = Target (TestSuite "test3") [] ["src", "tests"] [] [] [] False False
--    m = fromTargets [test, test2, specific, test3]
-- :}
--
-- >>> lookupBest "tests" m == [test, test2, test3]
-- True
--
-- >>> lookupBest "tests/specific" m == [specific]
-- True
--
-- >>> lookupBest "tests/specific/sub" m == [specific]
-- True
lookupBest :: FilePath -> TargetMap -> [Target]
lookupBest path trie = fromMaybe [] $ go (splitDirectories path) trie
  where go [] (TargetMap v _) = v
        go (p:ps) (TargetMap v c) = (M.lookup p c >>= go ps) <|> v

-- | Insert a target into the trie.
insert :: Target -> TargetMap -> TargetMap
insert target trie = foldl' union trie $ map (new . splitDirectories) $ sourceDirs target
  where new [] = TargetMap (Just [target]) M.empty
        new (p:ps) = TargetMap Nothing $ M.singleton p $ new ps

-- | Canonicalize the source directories of a target. 
canonicalizeTarget :: Target -> IO Target
canonicalizeTarget tgt = do
  sourceDirs' <- mapM canonicalizePath $ sourceDirs tgt
  return $ tgt { sourceDirs = sourceDirs' }

-- | Take the union of two target maps.
union :: TargetMap -> TargetMap -> TargetMap
union (TargetMap v c) (TargetMap v' c') = TargetMap (liftA2 mappend v v' <|> v <|> v') (M.unionWith union c c')

-- | A target map that contains no targets inside.
empty :: TargetMap
empty = TargetMap Nothing M.empty

-- | Create a target map from a list of targets
fromTargets :: [Target] -> TargetMap
fromTargets = foldl' (flip insert) empty
