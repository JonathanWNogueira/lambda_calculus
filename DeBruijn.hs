module DeBruijn where

import Data.List (elemIndex, (\\))
import Lambda (LTerm (evalStep))
import qualified Lambda as Lam (Term (), freeVars)

type Gamma = [Char]

data Term
  = Var Int         -- index
  | Abs Term        -- abstraction
  | App Term Term   -- aplication
  deriving (Eq)

instance Show Term where
  show :: Term -> String
  show (Var index) = show index
  show (Abs t) = "(lam . " ++ show t ++ ")"
  show (App s t) = "(" ++ show s ++ " " ++ show t ++ ")"

isValue :: Term -> Bool
isValue (Var _) = True
isValue (Abs _) = True  
isValue _ = False   -- aplication isnt value

removeNames :: Lam.Term -> Gamma -> Term    -- names to Bruijn
restoreNames :: Term -> Gamma -> Lam.Term   -- Bruijn to names
