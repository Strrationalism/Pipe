module Language.PipeScript.Parser.Expression (expr) where

import Language.PipeScript
import Language.PipeScript.Parser.Basic
import Text.Parsec

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
    

expandExpr :: Parser Expression
expandExpr = do
  choice 
    [ try $ DoubleExpandExpr <$> (string "~~" *> expr),
      ExpandExpr <$> (string "~" *> expr) ]

expr :: Parser Expression
expr =
  atomicExpr <|> expandExpr
