module New where

import System.Environment (getArgs)
import Text.ParserCombinators.Parsec

data LangVal
  = List [LangVal]
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
  case parse parseExpr "new-lang" input of
    Left err -> "No match: " ++ show err
    Right value -> "Found value: " ++ show value

parseExpr :: Parser LangVal
parseExpr =
  parseBool
    <|> parseString
    <|> parseNumber
    <|> parseList

parseBool :: Parser LangVal
parseBool = do
  x <- string "True" <|> string "False"
  return $ case x of
    "True" -> Bool True
    "False" -> Bool False
    _ -> Bool False

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

parseList :: Parser LangVal
parseList = do
  char '['
  x <- sepBy parseExpr (char ',' <* spaces)
  char ']'
  return $ List x

parseNumber :: Parser LangVal
parseNumber = Number . read <$> many1 digit

parseString :: Parser LangVal
parseString = do
  char '"'
  x <- many (noneOf "\"" <|> escaped)
  char '"'
  return $ String x

escaped :: Parser Char
escaped = char '\\' >> choice (zipWith escapedChar codes replacements)
  where
    codes = ['b', 'n', 'f', 'r', 't', '\\', '\"', '/']
    replacements = ['\b', '\n', '\f', '\r', '\t', '\\', '\"', '/']

escapedChar :: Char -> Char -> Parser Char
escapedChar code replacement = char code >> return replacement
