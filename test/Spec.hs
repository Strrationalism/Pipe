import Test.Hspec
import Test.QuickCheck
import TestParser (testParser)

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec $ do
  testParser
