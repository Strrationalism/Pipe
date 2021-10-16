import Test.Hspec
import Test.QuickCheck

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "add" $ do
    it "one plus one equals two" $ do
      (1 + 1) `shouldBe` 2
    it "exchange law" $ property $
      \a b -> (a :: Int) + b == b + a
