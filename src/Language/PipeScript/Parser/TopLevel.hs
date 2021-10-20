module Language.PipeScript.Parser.TopLevel (topLevelDef) where

import Language.PipeScript
import Language.PipeScript.Parser.Basic
import Language.PipeScript.Parser.Statement (stats)
import Text.Parsec

includeDef :: Parser TopLevel
includeDef = do
  string "include"
  ws1
  Include <$> stringConstant

platformFilterDef :: Parser PlatformFilter
platformFilterDef = do
  for <- option AnyPlatform $ string "for" *> ws1 *> platformSet
  ws1
  to <- option AnyPlatform $ string "to" *> ws1 *> platformSet
  return PlatformFilter {platformFor = for, platformTo = to}
  where
    platformSet = singlePlatform <|> multiPlatform
    singlePlatform = PlatformSet . (: []) <$> identifier
    multiPlatform = between (char '{' *> ws0) (ws0 *> char '}') $ do
      first <- optionMaybe identifier
      case first of
        Nothing -> return $ PlatformSet []
        Just x -> do
          more <- many $ ws0 *> char ',' *> ws0 *> identifier
          return $ PlatformSet $ x : more

topLevelBlockDef :: Parser title -> (title -> BlockDefination -> TopLevel) -> Parser TopLevel
topLevelBlockDef titleParser packer = do
  title <- titleParser
  ws1
  name <- identifier
  params <- many $ ws1 *> variable
  platFilter <- ws1 *> platformFilterDef
  lineEnd
  block <- stats
  let blockDef =
        BlockDefination
          { name = name,
            parameters = params,
            platformFilter = platFilter,
            block = block
          }
  return $ packer title blockDef

topLevelDef :: Parser TopLevel
topLevelDef = do
  char '-'
  ws0
  choice
    [ includeDef,
      topLevelBlockDef (string "task") $ const TaskDefination,
      topLevelBlockDef (string "action") $ const ActionDefination,
      try $
        topLevelBlockDef
          (string "operation" *> ws1 *> string "before")
          $ const $ OperationDefination BeforeAction,
      try $
        topLevelBlockDef
          (string "operation" *> ws1 *> string "after")
          $ const $ OperationDefination AfterAction,
      topLevelBlockDef (string "operation") $
        const $ OperationDefination NormalOperation
    ]