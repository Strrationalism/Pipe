module Language.PipeScript.Parser.Statement (stats) where

import Language.PipeScript
import Language.PipeScript.Parser.Basic
import Language.PipeScript.Parser.Expression (expr)
import Text.Parsec

exprStat :: Parser Statement
exprStat = ExprStat <$> expr

statBlock :: Parser [Statement]
statBlock = between (string "{" *> wsle0) (wsle0 *> string "}") $ try stats

ifStat :: Parser Statement
ifStat = do
  string "if"
  wsle1
  ifCondition <- expr
  wsle0
  ifBlock <- statBlock

  elseifs <- many $ try
    ( do
        wsle0
        string "else"
        wsle1
        string "if"
        wsle1
        elseifCondition <- expr
        wsle0
        b <- statBlock
        return (elseifCondition, b))

  elseBlock <-
    try $
      option
        []
        ( do
            wsle0
            string "else"
            wsle0
            statBlock
        )

  let branches = (ifCondition, ifBlock) : elseifs

  return $ IfStat branches elseBlock

stat :: Parser Statement
stat =
  choice
    [try ifStat, exprStat]

stats :: Parser [Statement]
stats = do
  wsle0
  first <- optionMaybe stat
  case first of
    Nothing -> return []
    Just x -> do
      next <- try $ option [] (wsle1 *> stats)
      return $ x : next
