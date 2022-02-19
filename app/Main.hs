module Main where

import Evaluator (eval)
import Parser (readExpr)
import System.Environment (getArgs)

-- main :: IO ()
-- main = do
--   args <- getArgs
--   case args of
--     [] -> putStrLn "Please pass an input to be parsed"
--     (expr : _) -> putStrLn (readExpr expr)

main :: IO ()
main = getArgs >>= print . eval . readExpr . head
