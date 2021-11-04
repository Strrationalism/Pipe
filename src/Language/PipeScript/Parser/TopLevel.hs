{-# LANGUAGE StrictData #-}
{-# LANGUAGE Strict #-}

module Language.PipeScript.Parser.TopLevel (topLevelDef) where

import Language.PipeScript
import Language.PipeScript.Parser.Basic
import Language.PipeScript.Parser.Statement (stats)
import Text.Parsec
import Control.Monad (void)

includeDef :: Parser TopLevel
includeDef = do
  string "include"
  ws1
  Include <$> stringConstant
  <?> "include"

platformFilterDef :: Parser PlatformFilter
platformFilterDef = do
  for <- option AnyPlatform (try (string "for") *> ws1 *> platformSet)
  ws0
  to <- option AnyPlatform (try (string "to") *> ws1 *> platformSet)
  return PlatformFilter {platformFor = for, platformTo = to}
  <?> "platform filter"
  where
    platformSet = singlePlatform <|> multiPlatform <?> "platform set"
    singlePlatform = PlatformSet . (: []) <$> identifier
    multiPlatform = between (char '{' *> ws0) (ws0 *> char '}') $ do
      first <- optionMaybe identifier
      case first of
        Nothing -> return $ PlatformSet []
        Just x -> do
          more <- many $ try (ws0 *> char ',' *> ws0 *> identifier)
          return $ PlatformSet $ x : more

topLevelBlockDef :: Parser () -> (BlockDefination -> TopLevel) -> Parser TopLevel
topLevelBlockDef titleParser packer = (<?> "top level defination") $ try $ do
  titleParser
  ws1
  name <- identifier
  params <- many $ try (ws1 *> variable)
  platFilter <- ws0 *> platformFilterDef
  mayHasBlock <- option False (True <$ lineEnd)
  block <- if mayHasBlock then option [] stats else return []
  let blockDef =
        BlockDefination
          { name = name,
            parameters = params,
            platformFilter = platFilter,
            block = block
          }
  return $ packer blockDef

topLevelDef :: Parser TopLevel
topLevelDef = (<?> "top level defination") $ try $ do
  char '-'
  ws1
  choice
    [ includeDef,
      topLevelBlockDef (void $ string "task") TaskDefination,
      try $ topLevelBlockDef (void $ string "action") ActionDefination,
      topLevelBlockDef
        (string "before" *> ws1 <* string "action")
        $ OperationDefination BeforeAction,
      try $
        topLevelBlockDef
          (string "after" *> ws1 <* string "action")
          $ OperationDefination AfterAction,
      topLevelBlockDef (void $ string "operation")
          $ OperationDefination NormalOperation
    ]
  