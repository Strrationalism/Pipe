module Language.PipeScript.Parser (Script, Language.PipeScript.Parser.parse) where

import Data.Text.IO
import Language.PipeScript
import Language.PipeScript.Parser.Basic
import Language.PipeScript.Parser.TopLevel (topLevelDef)
import Text.Parsec
import System.IO (openFile, utf8, hSetEncoding, hClose, IOMode (ReadMode))

parser :: Parser AST
parser = wsle0 *> many topLevelDef <* wsle0 <* eof

data Script = Script FilePath AST deriving (Eq, Read, Show)

parse :: FilePath -> IO (Either ParseError Script)
parse filePath =
  fmap (Script filePath) <$> ast
  where ast = Text.Parsec.parse parser filePath <$> text
        text = do
          handle <- openFile filePath ReadMode 
          hSetEncoding handle utf8
          input <- hGetContents handle
          hClose handle
          return input
          
        