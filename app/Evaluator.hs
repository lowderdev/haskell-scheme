module Evaluator where

import LispVal (LispVal (Atom, Bool, List, Number, String))

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args
eval _ = error "uh oh"

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
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
    ("number?", isLispNumber)
  ]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0

isLispSymbol :: [LispVal] -> LispVal
isLispSymbol [Atom _] = Bool True
isLispSymbol _ = Bool False

isLispString :: [LispVal] -> LispVal
isLispString [String _] = Bool True
isLispString _ = Bool False

isLispNumber :: [LispVal] -> LispVal
isLispNumber [Number _] = Bool True
isLispNumber _ = Bool False
