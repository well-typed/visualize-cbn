module CBN.Free (
    Count
  , Free(..)
  ) where

import Data.Map (Map)
import qualified Data.Map as Map

import CBN.Language
import qualified CBN.Util.Map as Map

type Count = Int

-- | Compute free variables
class Free a where
  free :: a -> Map Var Count

instance Free Var where
  free x = Map.singleton x 1

instance Free a => Free [a] where
  free = Map.unionsWith (+) . map free

instance Free Match where
  free (Match (Pat _ xs) e) = Map.deleteKeys xs $ free e

instance Free Term where
  free (TVar x)       = free x
  free (TApp e1 e2)   = free [e1, e2]
  free (TLam x e)     = Map.delete x $ free e
  free (TPtr _)       = Map.empty
  free (TCon _ es)    = free es
  free (TCase e ms)   = Map.unionWith (+) (free e) (free ms)
  free (TLet x e1 e2) = Map.delete x $ free [e1, e2]
  free (TPrim _ es)   = free es
  free (TIf c t f)    = free [c, t, f]
