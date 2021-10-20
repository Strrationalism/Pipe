{-# LANGUAGE LambdaCase #-}

module Language.PipeScript.Parser.Basic
  ( Parser (..),
    identifier,
    ws0,
    ws1,
    variable,
    boolConstant,
    intConstant,
    numberConstant,
    stringConstant,
    lineEnd,
    wsle1,
    wsle0,
    whiteSpaceOrLineEnd,
  )
where

import Control.Monad
import Data.Char (digitToInt)
import Data.Maybe (maybeToList)
import GHC.Base (Alternative (some))
import Language.PipeScript
import Text.Parsec
import Data.Text

type Parser = Parsec Text ()

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

boolConstant :: Parser Bool
boolConstant = do
  choice <- choice [string "true", string "false"]
  if choice == "true"
    then return True
    else return False

intConstant :: Parser Int
intConstant = do
  sig <- optionMaybe $ char '-'
  i <- some digitChar
  return $ read $ maybeToList sig ++ i

numberConstant :: Parser Double
numberConstant = do
  sig <- optionMaybe $ char '-'
  a <- some digitChar
  char '.'
  b <- many digitChar
  let bx = case b of
        [] -> "0"
        x -> x
  return $ read $ maybeToList sig ++ a ++ "." ++ bx

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
        '\\' -> return '\\'
        x -> fail $ "\\" ++ [x] ++ " is not a valid character."
    else return c

stringConstant :: Parser String
stringConstant = do
  char '\"'
  str <- many charInStringConstant
  char '\"'
  return str

comment :: Parser ()
comment =
  do
    char '#'
    void $ many $ satisfy (\x -> (x /= '\n') && (x /= '\r'))
    <?> "comment"

lineEnd :: Parser ()
lineEnd = do
  ws0
  void $ optionMaybe comment
  void endOfLine

whiteSpaceOrLineEnd :: Parser ()
whiteSpaceOrLineEnd = whiteSpace <|> lineEnd

wsle0 :: Parser ()
wsle0 = void $ many whiteSpaceOrLineEnd

wsle1 :: Parser ()
wsle1 = void $ some whiteSpaceOrLineEnd
