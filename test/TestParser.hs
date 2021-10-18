{-# LANGUAGE BlockArguments #-}
module TestParser (testParser) where

import Test.Hspec
import Test.QuickCheck

import Language.PipeScript
import Text.Parsec
import Language.PipeScript.Parser.Basic

test :: Parser a -> String -> Either ParseError a
test parser = parse parser "test"

-- Basic

basic :: Spec
basic =
  describe "Basic Parser" $ do
  it "identifier 1" $ do
    test identifier "abc" `shouldBe` Right (Identifier "abc")
  it "identifier 2" $ do
    test identifier "_123" `shouldBe` Right (Identifier "_123")
  it "identifier 3" $ do
    test identifier "a-123" `shouldBe` Right (Identifier "a-123")
  it "variable" $ do
    test variable "$a-123_super" `shouldBe` Right (Variable (Identifier "a-123_super"))
  it "bool true" $ do
    test bool "true" `shouldBe` Right True
  it "bool false" $ do
    test bool "false" `shouldBe` Right False
  it "int" $ property $
    \x -> test int (show x) == Right x
  it "int negative" $ property $
    \x -> test int (show $ negate x) == Right (negate x)
  it "number" $ do
    test number (show "1.234") `shouldBe` Right "1.234"
  it "number negative" $ do
    test number (show "-1.") `shouldBe` Right $ 0.0 - 1.0
  it "string constant" $ do
    test stringConstant "\"abc\\ndef\\n\"" `shouldBe` Right "abc\ndef\n"
  it "line end" $ do
    test lineEnd "" `shouldBe` Right ()
  it "line end with comment" $ do
    test lineEnd "# This is a super comment!!!   " `shouldBe` Right ()

-- Parser
testParser :: Spec
testParser = basic

