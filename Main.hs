module Main where

import DeBruijn ( removeNames, restoreNames )
import Lambda ( LTerm (eval) )
import qualified Lambda as Lam
import Parser ( parserlamb, lexer )

import Control.Monad.Writer (runWriter)

interpret :: String -> IO ()
interpret input = do
  let parsed = parserlamb $ lexer input -- syntax and lexical analysis
  let gamma = Lam.freeVars parsed       -- free vars from parsed

  let h = snd $ runWriter $ eval $ removeNames parsed gamma
  mapM_ (print . flip restoreNames gamma) h

main :: IO ()
main = do
  input <- getLine
  interpret input >> main
