module CBN.Subst (
    subst
  , RecursiveBinding(..)
  , allocSubst
  ) where

import Data.Map (Map)
import qualified Data.Map as Map

import CBN.Free
import CBN.Heap
import CBN.Language

-- | Substitution
--
-- NOTE: Although we deal with shadowing here @(\x -> .. (\x -> .. ))@, we
-- do NOT implement capture avoiding substitution. Since we never reduce
-- under binders, we can never have free variables, and hence this is not
-- something we need to worry about.
subst :: Var -> Term -> Term -> Term
subst x e' = go
  where
    go :: Term -> Term
    go (TVar x')       = if x == x' then e'
                                    else TVar x'
    go (TLam x' e)     = if x == x' then TLam x'     e
                                    else TLam x' (go e)
    go (TLet x' e1 e2) = if x == x' then TLet x'     e1      e2
                                    else TLet x' (go e1) (go e2)
    go (TApp e1 e2)    = TApp (go e1) (go e2)
    go (TPtr ptr')     = TPtr ptr'
    go (TCon c es)     = TCon c (map go es)
    go (TCase e ms)    = TCase (go e) (map goM ms)
    go (TPrim p es)    = TPrim p (map go es)

    goM :: Match -> Match
    goM (Match (Pat c xs) e) = if x `elem` xs then Match (Pat c xs)     e
                                              else Match (Pat c xs) (go e)

data RecursiveBinding = RecBinding | NonRecBinding

allocSubst :: RecursiveBinding -> [(Var, Term)] -> (Heap Term, Term) -> (Heap Term, Term)
allocSubst recBind = go
  where
    go :: [(Var, Term)] -> (Heap Term, Term) -> (Heap Term, Term)
    go []          (hp, e) = (hp, e)
    go ((x, s):ss) (hp, e)
      | isSimple s      = go ss (hp, subst x s e)
      | singleUse x s e = go ss (hp, subst x s e)
      | otherwise =
          let (hp', ptr) = alloc (Just (varName x)) hp (substRec x s)
              e'         = subst x (TPtr ptr) e
          in go ss (hp', e')

    -- Is this a "simple" term (one that we can substitute freely, even if
    -- multiple times)?
    isSimple :: Term -> Bool
    isSimple (TPtr _)     = True
    isSimple (TCon _ [])  = True
    isSimple (TPrim _ []) = True
    isSimple _            = False

    -- Is there (at most) only one use of this term?
    -- (If so, we substitute rather than allocate on the heap)
    -- If there are recursive occurrences we return False by definition.
    singleUse :: Var -> Term -> Term -> Bool
    singleUse x s e
      | RecBinding <- recBind, x `Map.member` free_s = False
      | otherwise = Map.findWithDefault 0 x free_e <= 1
      where
        free_s, free_e :: Map Var Count
        free_s = free s
        free_e = free e

    substRec :: Var -> Term -> Ptr -> Term
    substRec x s ptr | RecBinding <- recBind = subst x (TPtr ptr) s
                     | otherwise             = s
