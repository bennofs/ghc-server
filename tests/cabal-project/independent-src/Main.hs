import qualified Data.Text as T

main :: IO ()
main = return (return ()) bindings
  where bindings = T.null
