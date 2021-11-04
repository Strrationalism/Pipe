{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE Strict #-}

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
import Data.Text
import GHC.Base (Alternative (some))
import Language.PipeScript
import Text.Parsec

type Parser = Parsec Text ()

digitChar :: Parser Char
digitChar = oneOf ['0' .. '9']

notIdentifier :: [String]
notIdentifier =
  [ "true",
    "false",
    "for",
    "if",
    "in",
    "action",
    "task",
    "operation",
    "before",
    "after",
    "-"
  ]

identifier :: Parser Identifier
identifier = (<?> "identifier") $ try $ do
  first <- first
  next <- many next
  let iden = first : next
  if iden `elem` notIdentifier
    then fail "Identifier should not be a keyword."
    else return $ Identifier iden
  where
    first = choice [oneOf ['a' .. 'z'], oneOf ['A' .. 'Z'], char '_', char '-', char '.']
    next = choice [first, digitChar]

-- Whitespaces
whiteSpace :: Parser ()
whiteSpace = void $ oneOf [' ', '\t']

ws0 :: Parser ()
ws0 = void $ many whiteSpace

ws1 :: Parser ()
ws1 =
  void (some whiteSpace)
    <?> "whitespace"

variable :: Parser Variable
variable =
  do
    char '%'
    i <- identifier
    char '%'
    return $ Variable i
    <?> "variable"

boolConstant :: Parser Bool
boolConstant = (<?> "bool") $
  try $ do
    choice <- choice [string "true", string "false"]
    if choice == "true"
      then return True
      else return False

intConstant :: Parser Int
intConstant = (<?> "int") $ try $
  do
    sig <- optionMaybe $ char '-'
    i <- some digitChar
    return $ read $ maybeToList sig ++ i

numberConstant :: Parser Double
numberConstant = (<?> "number") $ try $
  do
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
stringConstant =
  do
    char '\"'
    str <- many charInStringConstant
    char '\"'
    return str
    <?> "string"

comment :: Parser ()
comment =
  do
    char '#'
    void $ many $ satisfy (\x -> (x /= '\n') && (x /= '\r'))
    <?> "comment"

lineEnd :: Parser ()
lineEnd =
  do
    ws0
    void $ optionMaybe comment
    void endOfLine
    <?> "line end"

whiteSpaceOrLineEnd :: Parser ()
whiteSpaceOrLineEnd = whiteSpace <|> lineEnd

wsle0 :: Parser ()
wsle0 = void $ many whiteSpaceOrLineEnd

wsle1 :: Parser ()
wsle1 =
  void (some whiteSpaceOrLineEnd)
    <?> "whitespace or line end"
