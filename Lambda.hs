module Lambda where

import Control.Monad.Writer ( Writer, runWriter, tell )
import Data.List ((\\))

data Term
  = Var Char                    -- value
  | Abs Char Term               -- abstraction
  | App Term Term deriving (Eq) -- aplication

instance Show Term where
  show :: Term -> String
  show (Var x) = [x]
  show (Abs x t) = "(lam " ++ [x] ++ " . " ++ show t ++ ")"
  show (App t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"

freeVars :: Term -> [Char]
freeVars (Var x) = [x]                            -- free variable
freeVars (Abs x t) = freeVars t \\ [x]            -- abstraction (remove x from t)
freeVars (App t1 t2) = freeVars t1 ++ freeVars t2 -- aplication (merge t1 & t2)

class (Show a, Eq a) => LTerm a where 
  eval :: a -> Writer [a] a
  evalStep :: a -> a

  eval a = do
    tell [a]                            -- log
    let b = evalStep a                  -- next state
    if a == b then return a else eval b -- evaluate till a nothing changes