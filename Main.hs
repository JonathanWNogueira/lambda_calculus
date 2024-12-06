module Main where

import Lambda ( LTerm (eval) )
import qualified Lambda as Lam
import Parser ( parserlamb, lexer )

interpret :: String -> IO ()
interpret input = do
  let parsed = parserlamb $ lexer input -- syntax and lexical analysis
  let gamma = Lam.freeVars parsed -- free vars from parsed


main :: IO ()
main = do
  input <- getLine
  interpret input >> main
