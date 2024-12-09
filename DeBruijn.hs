module DeBruijn where

import Data.List ( elemIndex, (\\) )
import Lambda ( LTerm ( evalStep ) )
import qualified Lambda as Lam ( Term ( Var, Abs, App ), freeVars )

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
  show (App t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"

removeNames :: Lam.Term -> Gamma -> Term    -- names to Bruijn
removeNames (Lam.Var name) gamma = 
  Var index where (Just index) = elemIndex name gamma     -- index
removeNames (Lam.Abs name t) gamma = 
  Abs $ removeNames t (name : gamma)                      -- recursively converting t into index
removeNames (Lam.App t1 t2) gamma = 
  App (removeNames t1 gamma) (removeNames t2 gamma)       -- recursively converting t1 & t2 into index

restoreNames :: Term -> Gamma -> Lam.Term   -- Bruijn to names
restoreNames (Var index) gamma = 
  Lam.Var $ gamma !! index                                -- name
restoreNames (Abs t) gamma = 
  Lam.Abs name (restoreNames t $ name : gamma)            -- recursively converting t into name
  where name = head $ ['a' ..] \\ gamma                   -- select from ['a' ..] removing gamma
restoreNames (App t1 t2) gamma =
  Lam.App (restoreNames t1 gamma)(restoreNames t2 gamma)  -- recursively converting t1 & t2 into names 

isValue :: Term -> Bool
isValue (Var _) = True
isValue (Abs _) = True  
isValue _ = False   -- aplication isnt value

shift :: Term -> Int -> Int -> Term
shift (Var index) d c = Var $ if index < c then index else (index + d)      -- index
shift (Abs t) d c = Abs $ (shift t d (c+1))                                 -- abstraction
shift (App t1 t2) d c = App (shift t1 d c) (shift t2 d c)                   -- aplication

subs :: Term -> Int -> Term -> Term
subs (Var index) j s = if index == j then s else (Var index)                -- index
subs (Abs t) j s = Abs $ subs t (j+1) (shift s 1 0)                         -- abstraction
subs (App t1 t2) j s = App (subs t1 j s) (subs t2 j s)                      -- aplication

betaReduction :: 