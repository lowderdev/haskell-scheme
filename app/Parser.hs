module Parser where

import Control.Monad (liftM)
import Control.Monad.Except
import LispVal
import Text.ParserCombinators.Parsec
  ( ParseError,
    Parser,
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

type ThrowsError = Either LispError

trapError :: (MonadError a m, Show a) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Left _) = error "extractValue called on error"
extractValue (Right val) = val

data LispError
  = NumArgs Integer [LispVal]
  | TypeMismatch String LispVal
  | Parser ParseError
  | BadSpecialForm String LispVal
  | NotFunction String String
  | UnboundVar String String
  | Default String

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) =
  "Expected " ++ show expected
    ++ " args; found values "
    ++ unwordsList found
showError (TypeMismatch expected found) =
  "Invalid type: expected " ++ expected
    ++ ", found "
    ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr
showError _ = error "todo"

instance Show LispError where show = showError

readExpr :: String -> ThrowsError LispVal
readExpr input =
  case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

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
