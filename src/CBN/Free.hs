module CBN.Free (
    Count
  , Free(..)
  , Pointers(..)
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

import CBN.Language
import CBN.Heap
import qualified CBN.Util.Map as Map

type Count = Int

{-------------------------------------------------------------------------------
  Free variables
-------------------------------------------------------------------------------}

-- | Compute free variables
class Free a where
  free :: a -> Map Var Count

instance Free Var where
  free x = Map.singleton x 1

instance Free a => Free [a] where
  free = Map.unionsWith (+) . map free

instance Free Match where
  free (Match (Pat _ xs) e) = Map.deleteKeys xs $ free e

instance Free ConApp where
  free (ConApp _ es) = free es

instance Free PrimApp where
  free (PrimApp _ es) = free es

instance Free Term where
  free (TVar x)       = free x
  free (TApp e1 e2)   = free [e1, e2]
  free (TLam x e)     = Map.delete x $ free e
  free (TPtr _)       = Map.empty
  free (TCon ces)     = free ces
  free (TCase e ms)   = Map.unionWith (+) (free e) (free ms)
  free (TLet x e1 e2) = Map.delete x $ free [e1, e2]
  free (TPrim pes)    = free pes
  free (TIf c t f)    = free [c, t, f]
  free (TSeq e1 e2)   = free [e1, e2]

{-------------------------------------------------------------------------------
  Used pointers
-------------------------------------------------------------------------------}

instance Pointers Ptr where
  pointers ptr = Set.singleton ptr

instance Pointers a => Pointers [a] where
  pointers = Set.unions . map pointers

instance Pointers Match where
  pointers (Match _pat e) = pointers e

instance Pointers ConApp where
  pointers (ConApp _ es) = pointers es

instance Pointers PrimApp where
  pointers (PrimApp _ es) = pointers es  

instance Pointers Term where
  pointers (TVar _)       = Set.empty
  pointers (TApp e1 e2)   = pointers [e1, e2]
  pointers (TLam _ e)     = pointers e
  pointers (TPtr ptr)     = pointers ptr
  pointers (TCon ces)     = pointers ces
  pointers (TCase e ms)   = Set.union (pointers e) (pointers ms)
  pointers (TLet _ e1 e2) = pointers [e1, e2]
  pointers (TPrim pes)    = pointers pes
  pointers (TIf c t f)    = pointers [c, t, f]
  pointers (TSeq e1 e2)   = pointers [e1, e2]
