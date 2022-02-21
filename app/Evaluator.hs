{-# LANGUAGE ExistentialQuantification #-}

module Evaluator where

import Control.Applicative
import Control.Monad.Except (MonadError (catchError, throwError))
import LispVal (LispVal (..))
import Parser (LispError (BadSpecialForm, NotFunction, NumArgs, TypeMismatch), ThrowsError)

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", pred, conseq, alt]) = do
  result <- eval pred
  case result of
    Bool False -> eval alt
    _ -> eval conseq
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval val@(List _) = return val
-- bad idea? should dottedlists be eval'd differently because they might
-- not terminate with the empty list?
eval val@(DottedList xs x) = eval $ List $ xs ++ [x]
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args =
  maybe
    (throwError $ NotFunction "Unrecognized primitive function args" func)
    ($ args)
    (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives =
  [ ("+", numericBinop (+)),
    ("-", numericBinop (-)),
    ("*", numericBinop (*)),
    ("/", numericBinop div),
    ("mod", numericBinop mod),
    ("quotient", numericBinop quot),
    ("remainder", numericBinop rem),
    ("symbol?", isLispSymbol),
    ("string?", isLispString),
    ("number?", isLispNumber),
    ("=", numBoolBinop (==)),
    ("<", numBoolBinop (<)),
    (">", numBoolBinop (>)),
    ("/=", numBoolBinop (/=)),
    (">=", numBoolBinop (>=)),
    ("<=", numBoolBinop (<=)),
    ("&&", boolBoolBinop (&&)),
    ("||", boolBoolBinop (||)),
    ("string=?", strBoolBinop (==)),
    ("string<?", strBoolBinop (<)),
    ("string>?", strBoolBinop (>)),
    ("string<=?", strBoolBinop (<=)),
    ("string>=?", strBoolBinop (>=)),
    ("car", car),
    ("cdr", cdr),
    ("cons", cons),
    ("eq?", eqv),
    ("eqv?", eqv),
    ("equal?", equal)
  ]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op [] = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpack op [x, y] = Bool <$> liftA2 op (unpack x) (unpack y)
boolBinop _ _ args = throwError $ NumArgs 2 args

numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinop = boolBinop unpackNum

strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinop = boolBinop unpackStr

boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinop = boolBinop unpackBool

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
-- type coersion
-- unpackNum (String n) =
--   let parsed = reads n
--    in if null parsed
--         then throwError $ TypeMismatch "number" $ String n
--         else return $ fst $ head parsed
-- unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String n) = pure n
-- type coersion
-- unpackStr (Number s) = return $ show s
-- unpackStr (Bool s)   = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool n) = pure n
unpackBool notBool = throwError $ TypeMismatch "bool" notBool

isLispSymbol :: [LispVal] -> ThrowsError LispVal
isLispSymbol [Atom _] = pure $ Bool True
isLispSymbol args = throwError $ NumArgs 1 args

isLispString :: [LispVal] -> ThrowsError LispVal
isLispString [String _] = pure $ Bool True
isLispString args = throwError $ NumArgs 1 args

isLispNumber :: [LispVal] -> ThrowsError LispVal
isLispNumber [Number _] = pure $ Bool True
isLispNumber args = throwError $ NumArgs 1 args

car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)] = return x
car [DottedList (x : xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [Bool x, Bool y] = return $ Bool $ x == y
eqv [Number x, Number y] = return $ Bool $ x == y
eqv [String x, String y] = return $ Bool $ x == y
eqv [Atom x, Atom y] = return $ Bool $ x == y
eqv [DottedList xs x, DottedList ys y] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [List x, List y] = return $ Bool $ (length x == length y) && all eqvPair (zip x y)
  where
    eqvPair (x1, x2) = case eqv [x1, x2] of
      Left lispErr -> False
      Right (Bool val) -> val
      Right _ -> error "eqv returned a non-bool value"
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
  primitiveEquals <- or <$> mapM (unpackEquals arg1 arg2) unpackerFunctions
  eqvEquals <- eqv [arg1, arg2]
  return $ Bool (primitiveEquals || let (Bool x) = eqvEquals in x)
  where
    unpackerFunctions = [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
equal badArgList = throwError $ NumArgs 2 badArgList

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
  do
    unpacked1 <- unpacker arg1
    unpacked2 <- unpacker arg2
    return $ unpacked1 == unpacked2
    `catchError` const (return False)
