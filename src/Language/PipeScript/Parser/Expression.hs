module Language.PipeScript.Parser.Expression (expr) where

import Language.PipeScript
import Language.PipeScript.Parser.Basic
import Text.Parsec
import Debug.Trace

atomicExpr :: Parser Expression
atomicExpr =
  choice
    [ try $ ConstantExpr . ConstNumber <$> numberConstant,
      ConstantExpr . ConstBool <$> boolConstant,
      ConstantExpr . ConstStr <$> stringConstant,
      ConstantExpr . ConstInt <$> intConstant,
      VariableExpr <$> variable,
      IdentifierExpr <$> identifier
    ]
  <?> "atomic expr"

expandExpr :: Parser Expression
expandExpr = do
  choice
    [ try $ DoubleExpandExpr <$> (string "~~" *> wsle0 *> wrappedExpr),
      ExpandExpr <$> (string "~" *> wsle0 *> wrappedExpr)
    ]
  <?> "expand expr"

listExpr :: Parser Expression
listExpr = 
  ListExpr <$> between (char '[' *> wsle0) (try (wsle0 *> char ']')) (exprList False)
  <?> "list expr"

applyExpr :: Bool -> Parser Expression
applyExpr isTopLevel = do
  a <- exprList isTopLevel
  case a of
    (left : right : rights) -> return $ ApplyExpr left $ right : rights
    [singleton] -> return singleton
    _ -> fail "Empty apply!"
  <?> "apply expr"

exprInner :: Bool -> Parser Expression
exprInner isTopLevel =
  choice
    [ applyExpr isTopLevel,
      atomicExpr,
      expandExpr,
      wrappedExpr,
      listExpr
    ]

wrappedExpr :: Parser Expression
wrappedExpr =
  choice
    [ between (char '(' *> wsle0) (wsle0 *> char ')') $ exprInner False,
      atomicExpr,
      expandExpr,
      listExpr
    ]

exprList :: Bool -> Parser [Expression]
exprList isTopLevel = do
  let ws1c = if isTopLevel then ws1 else wsle1
  first <- optionMaybe wrappedExpr
  case first of
    Nothing -> return []
    Just x -> (x :) <$> many (try (ws1c *> wrappedExpr))
  <?> "expr list"

expr :: Parser Expression
expr = exprInner True

