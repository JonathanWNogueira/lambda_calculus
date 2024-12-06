module Lambda where

import Data.List ((\\))

data Term
  = Var Char
  | Abs Char Term
  | App Term Term deriving (Eq)

instance Show Term where
  show :: Term -> String
  show (Var x) = [x]
  show (Abs x t) = "(lam " ++ [x] ++ " . " ++ show t ++ ")"
  show (App t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"

freeVars :: Term -> [Char]
freeVars (Var x) = [x]
freeVars (Abs x t) = freeVars t \\ [x]
freeVars (App t1 t2) = freeVars t1 ++ freeVars t2