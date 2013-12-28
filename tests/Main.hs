import Test.Tasty
import qualified Admin
import qualified Check

main :: IO ()
main = defaultMain $ testGroup "ghc-server"
  [ Admin.tests
  , Check.tests
  ]
