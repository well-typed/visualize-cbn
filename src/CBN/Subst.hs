module CBN.Subst (
    substPtr
  , RecursiveBinding(..)
  , allocSubst
  ) where

import CBN.Language
import CBN.Heap

substPtr :: Var -> Ptr -> Term -> Term
substPtr x ptr = go
  where
    go :: Term -> Term
    go (TVar x')       = if x == x' then TPtr ptr
                                    else TVar x'
    go (TLam x' e)     = if x == x' then TLam x'     e
                                    else TLam x' (go e)
    go (TLet x' e1 e2) = if x == x' then TLet x'     e1      e2
                                    else TLet x' (go e1) (go e2)
    go (TApp e1 e2)    = TApp (go e1) (go e2)
    go (TPtr ptr')     = TPtr ptr'
    go (TCon c es)     = TCon c (map go es)
    go (TPat e ms)     = TPat (go e) (map goM ms)
    go (TPrim p es)    = TPrim p (map go es)

    goM :: Match -> Match
    goM (Match (Pat c xs) e) = if x `elem` xs then Match (Pat c xs)     e
                                              else Match (Pat c xs) (go e)

data RecursiveBinding = RecursiveBinding | NonRecursiveBinding

allocSubst :: RecursiveBinding -> [(Var, Term)] -> (Heap Term, Term) -> (Heap Term, Term)
allocSubst recBind = go
  where
    go :: [(Var, Term)] -> (Heap Term, Term) -> (Heap Term, Term)
    go []          (hp, e) = (hp, e)
    go ((x, s):ss) (hp, e) =
      case s of
        TPtr ptr   -> go ss (hp, substPtr x ptr e)
        _otherwise -> let (hp', ptr) = alloc (Just (varName x)) hp (substRec x s)
                          e'         = substPtr x ptr e
                      in go ss (hp', e')

    substRec :: Var -> Term -> Ptr -> Term
    substRec x s ptr | RecursiveBinding <- recBind = substPtr x ptr s
                     | otherwise                   = s
