module TestParser (testParser) where

import Data.Functor.Identity (Identity (Identity))
import Data.Text
import GHC.Unicode (toLower)
import Language.PipeScript
import Language.PipeScript.Parser.Basic
import Language.PipeScript.Parser.Expression (expr)
import Language.PipeScript.Parser.Statement (stats)
import Language.PipeScript.Parser.TopLevel (topLevelDef)
import Test.Hspec
import Test.QuickCheck
import Text.Parsec

test :: Parser a -> String -> Either ParseError a
test parser = parse parser "test" . pack

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

    it "identifier 4" $ do
      test identifier "-abc" `shouldBe` Right (Identifier "-abc")

    it "variable" $ do
      test variable "%a-123_super%" `shouldBe` Right (Variable (Identifier "a-123_super"))

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
      test wsle1 "#test \n \n   \n" `shouldBe` Right ()

    it "ws0" $ do
      test ws0 "" `shouldBe` Right ()

    it "ws1" $ do
      test ws1 " " `shouldBe` Right ()

-- Expression
expression :: Spec
expression =
  describe "Expression Parser" $ do
    it "AtomicExpr Identifier" $ do
      test expr "abc" `shouldBe` Right (IdentifierExpr $ Identifier "abc")

    it "AtomicExpr Variable" $ do
      test expr "%abc%" `shouldBe` Right (VariableExpr $ Variable $ Identifier "abc")

    it "AtomicExpr Constant Str" $ do
      test expr "\"abc\"" `shouldBe` Right (ConstantExpr $ ConstStr "abc")

    it "AtomicExpr Constant Int" $
      property $
        \x -> test expr (show x) `shouldBe` Right (ConstantExpr $ ConstInt x)

    it "AtomicExpr Constant Number" $ do
      test expr "12345." `shouldBe` Right (ConstantExpr $ ConstNum 12345.0)

    it "AtomicExpr Constant Bool" $ do
      test expr "true" `shouldBe` Right (ConstantExpr $ ConstBool True)
      test expr "false" `shouldBe` Right (ConstantExpr $ ConstBool False)

    it "AtomicExpr Expand" $ do
      test expr "~\"abc\"" `shouldBe` Right (ExpandExpr $ ConstantExpr $ ConstStr "abc")

    it "AtomicExpr DoubleExpand" $ do
      test expr "~~\"abc\"" `shouldBe` Right (DoubleExpandExpr $ ConstantExpr $ ConstStr "abc")

    it "ListExpr 1" $ do
      test expr "[([\n 1#Test \n 2 #test \n 3 \n ])]"
        `shouldBe` Right
          ( ListExpr
              [ ListExpr
                  [ ConstantExpr $ ConstInt 1,
                    ConstantExpr $ ConstInt 2,
                    ConstantExpr $ ConstInt 3
                  ]
              ]
          )

    it "ApplyExpr 1" $ do
      test expr "1 \n 2 3"
        `shouldNotBe` Right
          ( ApplyExpr
              (ConstantExpr $ ConstInt 1)
              [ConstantExpr $ ConstInt 2, ConstantExpr $ ConstInt 3]
          )

    it "ApplyExpr 2" $ do
      test expr "1 2 3"
        `shouldBe` Right
          ( ApplyExpr
              (ConstantExpr $ ConstInt 1)
              [ConstantExpr $ ConstInt 2, ConstantExpr $ ConstInt 3]
          )

    it "ApplyExpr 3" $ do
      test expr "(set (add 1))"
        `shouldBe` Right
          ( ApplyExpr
              (IdentifierExpr $ Identifier "set")
              [ApplyExpr (IdentifierExpr $ Identifier "add") [ConstantExpr $ ConstInt 1]]
          )

    it "ApplyExpr 4" $ do
      test expr "set (tail ttfFile)"
        `shouldBe` Right
          ( ApplyExpr
              (IdentifierExpr $ Identifier "set")
              [ApplyExpr (IdentifierExpr $ Identifier "tail") [IdentifierExpr $ Identifier "ttfFile"]]
          )

    it "ComplexExpr 1" $ do
      test expr "[([1 \n  (2  \n 3  \n 4 \n  ) \n ( \n 3 \n )])]"
        `shouldBe` Right
          ( ListExpr
              [ ListExpr
                  [ ConstantExpr $ ConstInt 1,
                    ApplyExpr
                      (ConstantExpr $ ConstInt 2)
                      [ ConstantExpr $ ConstInt 3,
                        ConstantExpr $ ConstInt 4
                      ],
                    ConstantExpr $ ConstInt 3
                  ]
              ]
          )

-- Statements
statement :: Spec
statement =
  describe "Statement Parser" $ do
    it "Simple Statement" $ do
      test stats "mkdir \"abc\""
        `shouldBe` Right
          [ ExprStat
              ( ApplyExpr
                  (IdentifierExpr $ Identifier "mkdir")
                  [ConstantExpr $ ConstStr "abc"]
              )
          ]

    it "If Statement" $ do
      test stats "if add { \n    mkdir }"
        `shouldBe` Right
          [ IfStat
              [ ( IdentifierExpr $ Identifier "add",
                  [ExprStat $ IdentifierExpr (Identifier "mkdir")]
                )
              ]
              []
          ]

    it "If-Else Statement" $ do
      test stats "if add { \n     } else { mkdir }"
        `shouldBe` Right
          [ IfStat
              [(IdentifierExpr $ Identifier "add", [])]
              [ExprStat $ IdentifierExpr (Identifier "mkdir")]
          ]

    it "If-ElseIf-Else Statement" $ do
      test stats "if add { \n     } else if sub { mkdir } else {\n\n\n\n\n\n\n}"
        `shouldBe` Right
          [ IfStat
              [ (IdentifierExpr $ Identifier "add", []),
                (IdentifierExpr $ Identifier "sub", [ExprStat $ IdentifierExpr (Identifier "mkdir")])
              ]
              []
          ]

-- TopLevel
topLevel :: Spec
topLevel =
  describe "Top Level Parser" $ do
    it "Include" $ do
      test topLevelDef "- include \"abc.pipe\""
        `shouldBe` Right (Include "abc.pipe")
    it "Complex" $ do
      test topLevelDef "- before action super %abc% %def% for Windows to { Linux, macOS, Windows }\nmkdir"
        `shouldBe` Right
          ( OperationDefination
              BeforeAction
              ( BlockDefination
                  { name = Identifier "super",
                    parameters =
                      [ Variable (Identifier "abc"),
                        Variable (Identifier "def")
                      ],
                    platformFilter =
                      PlatformFilter
                        { platformFor =
                            PlatformSet [Identifier "Windows"],
                          platformTo =
                            PlatformSet
                              [ Identifier "Linux",
                                Identifier "macOS",
                                Identifier "Windows"
                              ]
                        },
                    block = [ExprStat $ IdentifierExpr $ Identifier "mkdir"]
                  }
              )
          )

    it "No Block" $ do
      test topLevelDef "- action super %abc% %def% to {}"
        `shouldBe` Right
          ( ActionDefination
              ( BlockDefination
                  { name = Identifier "super",
                    parameters = [Variable (Identifier "abc"), Variable (Identifier "def")],
                    platformFilter =
                      PlatformFilter
                        { platformFor = AnyPlatform,
                          platformTo = PlatformSet []
                        },
                    block = []
                  }
              )
          )
    it "No Block 2" $ do
      test topLevelDef "- task super %abc% %def% for {}\n"
        `shouldBe` Right
          ( TaskDefination
              ( BlockDefination
                  { name = Identifier "super",
                    parameters =
                      [ Variable (Identifier "abc"),
                        Variable (Identifier "def")
                      ],
                    platformFilter =
                      PlatformFilter
                        { platformFor = PlatformSet [],
                          platformTo = AnyPlatform
                        },
                    block = []
                  }
              )
          )

-- Parser
testParser :: Spec
testParser = do
  basic
  expression
  statement
  topLevel
