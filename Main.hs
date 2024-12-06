module Main where

import qualified Lambda as Lam
import Parser (lexer, parserlamb)

interpret :: String -> IO ()
interpret input = do
  let parsed = parserlamb $ lexer input -- syntax and lexical analysis
  let gamma = Lam.freeVars parsed


main :: IO ()
main = do
  input <- getLine
  interpret input >> main
