module Main where

import System.Environment(getArgs)
import Declarations
import Grammar (parse)
import Formatter (format)
import Calculator (simplify)
import Graphs (environment)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [str] -> putStrLn (program environment str)
    _     -> putStrLn "Error"

program :: Environment -> String -> String
program env str
  = case ete of
      Left te -> format (simplify env te)
      Right str -> str
  where
    (ete, msg) = parse str
