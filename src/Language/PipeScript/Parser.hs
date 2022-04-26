module Language.PipeScript.Parser
  ( Script,
    parsePipeScript,
    parsePipeScriptWithIncludes,
    onlyThisPlatform,
    scriptAST,
    scriptPath,
    scriptDir
  )
where

import Control.Monad
import Data.Text.IO
import Language.PipeScript
import Language.PipeScript.Parser.Basic
import Language.PipeScript.Parser.TopLevel (topLevelDef)
import System.IO (IOMode (ReadMode), hClose, hSetEncoding, openFile, utf8)
import Text.Parsec
import System.FilePath

parser :: Parser AST
parser = many (wsle0 *> topLevelDef <* wsle0) <* eof

data Script = Script FilePath AST deriving (Eq, Show)

scriptPath :: Script -> FilePath
scriptPath (Script path _) = path

scriptDir :: Script -> FilePath 
scriptDir = takeDirectory . scriptPath

scriptAST :: Script -> AST
scriptAST (Script _ ast) = ast

parsePipeScript :: FilePath -> IO (Either ParseError Script)
parsePipeScript path =
  fmap (Script path) <$> ast
  where
    ast = Text.Parsec.parse parser path <$> text
    text = do
      handle <- openFile path ReadMode
      hSetEncoding handle utf8
      input <- hGetContents handle
      hClose handle
      return input

parsePipeScriptWithIncludes :: FilePath -> IO [Either ParseError Script]
parsePipeScriptWithIncludes path = do
  my <- parsePipeScript path
  case my of
    Left x -> return [Left x]
    Right script -> 
      (my :) <$> parseIncludes (scriptAST script)
      where
        parseInclude (Include x) = do
          let subScriptPath = scriptDir script </> x
          parsePipeScriptWithIncludes subScriptPath
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
    f (Include _) = False

onlyThisPlatform :: String -> String -> [Script] -> [Script]
onlyThisPlatform for to scrs = f <$> scrs
  where
    f (Script s ast) = Script s $ topLevelsOnlyThisPlatform for to ast