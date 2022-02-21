module Main where

import Evaluator (eval)
import Parser (extractValue, readExpr, trapError)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let evaled = fmap show $ readExpr (head args) >>= eval
  putStrLn $ extractValue $ trapError evaled
