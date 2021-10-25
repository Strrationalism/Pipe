module Language.PipeScript.Parser.Expression (expr) where

import Language.PipeScript
import Language.PipeScript.Parser.Basic
import Text.Parsec
import Debug.Trace

atomicExpr :: Parser Expression
atomicExpr = (<?> "atomic expr") $ try $ choice
  [ try $ ConstantExpr . ConstNum <$> numberConstant,
    ConstantExpr . ConstBool <$> boolConstant,
    ConstantExpr . ConstStr <$> stringConstant,
    try $ ConstantExpr . ConstInt <$> intConstant,
    VariableExpr <$> variable,
    try $ IdentifierExpr <$> identifier
  ]
  

expandExpr :: Parser Expression
expandExpr = (<?> "expand expr") $ try $ do
  choice
    [ try $ DoubleExpandExpr <$> (string "~~" *> wsle0 *> wrappedExpr),
      ExpandExpr <$> (string "~" *> wsle0 *> wrappedExpr)
    ]

listExpr :: Parser Expression
listExpr = (<?> "list expr") $ try $ 
  ListExpr <$> between (char '[' *> wsle0) (try (wsle0 *> char ']')) (exprList False)

applyExpr :: Bool -> Parser Expression
applyExpr isTopLevel = (<?> "apply expr") $ try $ do
  a <- exprList isTopLevel
  case a of
    (left : right : rights) -> return $ ApplyExpr left $ right : rights
    [singleton] -> return singleton
    _ -> fail "Empty apply!"

exprInner :: Bool -> Parser Expression
exprInner isTopLevel =
  try $ choice
    [ applyExpr isTopLevel,
      atomicExpr,
      expandExpr,
      wrappedExpr,
      listExpr
    ]

wrappedExpr :: Parser Expression
wrappedExpr =
  try $ choice
    [ between (char '(' *> wsle0) (wsle0 *> char ')') $ exprInner False,
      atomicExpr,
      expandExpr,
      listExpr
    ]

exprList :: Bool -> Parser [Expression]
exprList isTopLevel = (<?> "expr list") $ try $ do
  let ws1c = if isTopLevel then ws1 else wsle1
  first <- optionMaybe wrappedExpr
  case first of
    Nothing -> return []
    Just x -> (x :) <$> many (try (ws1c *> wrappedExpr))

expr :: Parser Expression
expr = exprInner True

