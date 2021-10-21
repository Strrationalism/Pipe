module Language.PipeScript.Parser
  ( Script,
    parsePipeScript,
    parsePipeScriptWithIncludes,
    onlyThisPlatform,
  )
where

import Control.Monad
import Data.Text.IO
import Language.PipeScript
import Language.PipeScript.Parser.Basic
import Language.PipeScript.Parser.TopLevel (topLevelDef)
import System.IO (IOMode (ReadMode), hClose, hSetEncoding, openFile, utf8)
import Text.Parsec

parser :: Parser AST
parser = wsle0 *> many topLevelDef <* wsle0 <* eof

data Script = Script FilePath AST deriving (Eq, Read, Show)

parsePipeScript :: FilePath -> IO (Either ParseError Script)
parsePipeScript filePath =
  fmap (Script filePath) <$> ast
  where
    ast = Text.Parsec.parse parser filePath <$> text
    text = do
      handle <- openFile filePath ReadMode
      hSetEncoding handle utf8
      input <- hGetContents handle
      hClose handle
      return input

parsePipeScriptWithIncludes :: FilePath -> IO [Either ParseError Script]
parsePipeScriptWithIncludes filePath = do
  my <- parsePipeScript filePath
  case my of
    Left x -> return [Left x]
    Right (Script _ ast) -> (my :) <$> parseIncludes ast
  where
    parseInclude (Include x) = join <$> sequence [parsePipeScriptWithIncludes x]
    parseInclude _ = return []
    parseIncludes ast = fmap join $ sequence $ parseInclude <$> (ast :: AST)

isPlatformInFilter :: String -> String -> PlatformFilter -> Bool
isPlatformInFilter for to filter =
  ok for (platformFor filter) && ok to (platformTo filter)
  where
    ok _ AnyPlatform = True
    ok x (PlatformSet y) = Identifier x `elem` y

topLevelsOnlyThisPlatform :: String -> String -> AST -> AST
topLevelsOnlyThisPlatform for to = filter f
  where
    f (OperationDefination _ def) = isPlatformInFilter for to $ platformFilter def
    f (TaskDefination def) = isPlatformInFilter for to $ platformFilter def
    f (ActionDefination def) = isPlatformInFilter for to $ platformFilter def
    f (Include _) = True

onlyThisPlatform :: String -> String -> [Script] -> [Script]
onlyThisPlatform for to scrs = f <$> scrs
  where
    f (Script s ast) = Script s $ topLevelsOnlyThisPlatform for to ast