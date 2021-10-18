{-# LANGUAGE LambdaCase #-}

module Language.PipeScript.Parser.Basic
  ( Parser (..),
    identifier,
    ws0,
    ws1,
    variable,
    bool,
    int,
    number,
    stringConstant,
    lineEnd,
  )
where

import Control.Monad
import Data.Char (digitToInt)
import Data.Maybe (maybeToList)
import GHC.Base (Alternative (some))
import Language.PipeScript
import Text.Parsec

type Parser = Parsec String ()

digitChar :: Parser Char
digitChar = oneOf ['0' .. '9']

identifier :: Parser Identifier
identifier = do
  first <- first
  next <- many next
  return $ Identifier (first : next)
  where
    first = choice [oneOf ['a' .. 'z'], oneOf ['A' .. 'Z'], char '_']
    next = choice [first, digitChar, char '-']

-- Whitespaces
whiteSpace :: Parser ()
whiteSpace = void $ oneOf [' ', '\t']

ws0 :: Parser ()
ws0 = void $ many whiteSpace

ws1 :: Parser ()
ws1 = void $ some whiteSpace

variable :: Parser Variable
variable = do
  char '$'
  ws0
  Variable <$> identifier

bool :: Parser Bool
bool = do
  choice <- choice [string "true", string "false"]
  if choice == "true"
    then return True
    else return False

int :: Parser Int
int = do
  sig <- optionMaybe $ char '-'
  i <- some digitChar
  return $ read $ maybeToList sig ++ i

number :: Parser Float
number = do
  sig <- optionMaybe $ char '-'
  a <- some digitChar
  char '.'
  b <- many digitChar
  return $ read $ maybeToList sig ++ a ++ "." ++ b

charInStringConstant :: Parser Char
charInStringConstant = do
  c <- satisfy (/= '\"')
  if c == '\\'
    then
      satisfy (const True) >>= \case
        'n' -> return '\n'
        't' -> return '\t'
        'r' -> return '\r'
        '\"' -> return '\"'
        '\'' -> return '\''
        x -> fail $ "\\" ++ [x] ++ " is not a valid character."
    else return c

stringConstant :: Parser String
stringConstant = do
  char '\"'
  str <- many charInStringConstant
  char '\"'
  return str

comment :: Parser ()
comment = do
  char '#'
  void $ many $ satisfy (\x -> (x /= '\n') && (x /= '\r'))

lineEnd :: Parser ()
lineEnd = do
  ws0
  void $ optionMaybe comment
  choice [void endOfLine, eof]