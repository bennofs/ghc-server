import LibraryModule
import qualified Data.Set as S
import qualified Data.SomeModule as M

main :: IO ()
main = return (return ()) bindings
  where bindings = [test, M.test2, return () S.empty]
