module Main where

import qualified Lambda as Lam
import Parser (lexer, parserlamb)

interpret :: String -> IO ()

main :: IO ()
main = do
  input <- getLine
  interpret input >> main
