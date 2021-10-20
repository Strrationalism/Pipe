import Test.Hspec
import Test.QuickCheck
import TestParser (testParser)

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
doHspec :: IO ()
doHspec = hspec $ do
  testParser

main :: IO ()
main = do
  doHspec
