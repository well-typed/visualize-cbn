module CBN.Free (Free(..)) where

-- | Compute free variables
class Free a where
  free :: a -> Map Var Count

instance Free Var where
  free x = Map.singleton x 1

instance Free a => Free [a] where
  free = Map.unionsWith (+) . map free

instance Free Match where
  free (Match (Pat _ xs) e) = deleteKeys xs $ free e

instance Free Term where
  free (TVar x)     = free x
  free (TApp e1 e2) = free [e1, e2]
  free (TLam x e)   = Map.delete x $ free e
  free (TPtr _)     = Map.empty
  free (TCon _ es)  = free es
  free (TPat e ms)  = Map.unionWith (+) (free e) (free ms)
