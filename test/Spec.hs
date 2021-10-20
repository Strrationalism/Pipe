import Language.PipeScript.Parser
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
  putStrLn "= Test Load Example ="
  p <- parsePipeScript "./test/Example.pipe"
  case p of
    Left x -> print x
    Right x -> print x
  putStrLn ""
  putStrLn "= Unit Tests ="
  doHspec
