module Main where

import Evaluator (eval)
import Parser (extractValue, readExpr, trapError)
import System.Environment (getArgs)
import System.IO

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> runRepl
    1 -> evalAndPrint $ head args
    _ -> putStrLn "Program takes only 0 or 1 argument"

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "Lisp>>> ") evalAndPrint

evalAndPrint :: String -> IO ()
evalAndPrint expr = evalString expr >>= putStrLn

evalString :: String -> IO String
evalString expr = return $ extractValue $ trapError (fmap show $ readExpr expr >>= eval)

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
    then return ()
    else action result >> until_ pred prompt action
