import Test.Hspec
import Test.QuickCheck
import TestParser (testParser)
import Language.PipeScript.Parser

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
doHspec :: IO ()
doHspec = hspec $ do
  testParser

main :: IO ()
main = do
  putStrLn "= Test Load Example ="
  p <- parse "./test/Example.pipe"
  case p of Left x -> print x
            Right x -> print x
  putStrLn ""
  putStrLn "= Unit Tests ="
  doHspec
