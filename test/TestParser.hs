module TestParser (testParser) where

import Language.PipeScript
import Language.PipeScript.Parser.Basic
import Language.PipeScript.Parser.Expression (expr)
import Test.Hspec
import Test.QuickCheck
import Text.Parsec
import GHC.Unicode (toLower)

test :: Parser a -> String -> Either ParseError a
test parser = parse parser "test"

-- Basic
basic :: SpecWith ()
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
      test boolConstant "true" `shouldBe` Right True

    it "bool false" $ do
      test boolConstant "false" `shouldBe` Right False

    it "int" $
      property $
        \x -> test intConstant (show x) == Right x

    it "int negative" $
      property $
        \x -> test intConstant (show $ negate x) == Right (negate x)

    it "number" $ do
      test numberConstant "1.234" `shouldBe` Right 1.234

    it "number negative" $ do
      test numberConstant "-1." `shouldBe` Right (-1.0)

    it "string constant" $ do
      test stringConstant "\"abc\\ndef\\n\"" `shouldBe` Right "abc\ndef\n"

    it "line end" $ do
      test lineEnd "\r\n" `shouldBe` Right ()

    it "line end with comment" $ do
      test lineEnd "# This is a super comment!!!   \n" `shouldBe` Right ()

    it "wsle0" $ do
      test wsle0 "" `shouldBe` Right ()

    it "wsle1" $ do
      test wsle1 " " `shouldBe` Right ()

    it "ws0" $ do
      test ws0 "" `shouldBe` Right ()

    it "ws1" $ do
      test ws1 " " `shouldBe` Right ()

-- Expression
expression :: SpecWith ()
expression =
  describe "Expression Parser" $ do
    it "AtomicExpr Identifier" $ do
      test expr "abc" `shouldBe` Right (IdentifierExpr $ Identifier "abc")

    it "AtomicExpr Variable" $ do
      test expr "$abc" `shouldBe` Right (VariableExpr $ Variable $ Identifier "abc")

    it "AtomicExpr Constant Str" $ do
      test expr "\"abc\"" `shouldBe` Right (ConstantExpr $ ConstStr "abc")

    it "AtomicExpr Constant Int" $ property $
      \x -> test expr (show x) `shouldBe` Right (ConstantExpr $ ConstInt x)

    it "AtomicExpr Constant Number" $ do
        test expr "12345." `shouldBe` Right (ConstantExpr $ ConstNumber 12345.0)

    it "AtomicExpr Constant Bool" $ property $
      \x -> test expr (toLower <$> show x) `shouldBe` Right (ConstantExpr $ ConstBool x)

    it "AtomicExpr Expand" $ do
      test expr "~\"abc\"" `shouldBe` Right (ExpandExpr $ ConstantExpr $ ConstStr "abc")

    it "AtomicExpr DoubleExpand" $ do
      test expr "~~\"abc\"" `shouldBe` Right (DoubleExpandExpr $ ConstantExpr $ ConstStr "abc")

-- Parser
testParser = do
  basic
  expression
  
