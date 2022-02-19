module Main where

import Control.Monad (liftM)
import System.Environment (getArgs)
import Text.ParserCombinators.Parsec
  ( Parser,
    char,
    choice,
    digit,
    endBy,
    letter,
    many,
    many1,
    noneOf,
    oneOf,
    parse,
    sepBy,
    skipMany1,
    space,
    string,
    try,
    (<|>),
  )

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool
  deriving (Show)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Please pass an input to be parsed"
    (expr : _) -> putStrLn (readExpr expr)

readExpr :: String -> String
readExpr input =
  case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right value -> "Found value: " ++ show value

parseExpr :: Parser LispVal
parseExpr =
  parseAtom
    <|> parseString
    <|> parseNumber
    <|> parseQuoted
    <|> do
      char '('
      x <- try parseList <|> parseDottedList
      char ')'
      return x

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _ -> Atom atom

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseNumber :: Parser LispVal
parseNumber = Number . read <$> many1 digit

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (noneOf "\"" <|> escaped)
  char '"'
  return $ String x

spaces :: Parser ()
spaces = skipMany1 space

-- specialCharacter :: Parser Char
-- specialCharacter = char '\\' *> oneOf "\"nrt\\"

escaped :: Parser Char
escaped = char '\\' >> choice (zipWith escapedChar codes replacements)
  where
    codes = ['b', 'n', 'f', 'r', 't', '\\', '\"', '/']
    replacements = ['\b', '\n', '\f', '\r', '\t', '\\', '\"', '/']

escapedChar :: Char -> Char -> Parser Char
escapedChar code replacement = char code >> return replacement

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]
