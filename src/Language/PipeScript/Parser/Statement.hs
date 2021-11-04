{-# LANGUAGE StrictData #-}
{-# LANGUAGE Strict #-}

module Language.PipeScript.Parser.Statement (stats) where

import Debug.Trace
import Language.PipeScript
import Language.PipeScript.Parser.Basic
import Language.PipeScript.Parser.Expression (expr)
import Text.Parsec

exprStat :: Parser Statement
exprStat = ExprStat <$> expr

statBlock :: Parser [Statement]
statBlock =
  between (string "{" *> wsle0) (wsle0 *> string "}") (try stats)
    <?> "block"

forEachLoop :: Parser Statement
forEachLoop = do
  string "for"
  wsle1
  loopVar <- variable
  wsle1
  string "in"
  wsle1
  ls <- expr
  wsle0
  ForEachLoop loopVar ls <$> statBlock


ifStat :: Parser Statement
ifStat = do
  string "if"
  wsle1
  ifCondition <- expr
  wsle0
  ifBlock <- statBlock
  elseifs <-
    many $
      try
        ( do
            wsle0
            string "else"
            wsle1
            string "if"
            wsle1
            elseifCondition <- expr
            wsle0
            b <- statBlock
            return (elseifCondition, b)
        )

  elseBlock <-
    option [] $
      try
        ( do
            wsle0
            string "else"
            wsle0
            statBlock
        )

  let branches = (ifCondition, ifBlock) : elseifs
  return $ IfStat branches elseBlock
  <?> "if statement"

stat :: Parser Statement
stat =
  (<?> "statement") $ try $ choice
    [ try forEachLoop, try ifStat, exprStat]

stats :: Parser [Statement]
stats = (<?> "statements") $ try $
  do
    wsle0
    first <- optionMaybe stat
    case first of
      Nothing -> return []
      Just x -> do
        next <- option [] $ try (wsle1 *> stats)
        return $ x : next

