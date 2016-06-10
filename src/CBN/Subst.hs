module CBN.Subst (
    substPtr
  , allocSubst
  ) where

import CBN.Language
import CBN.Heap

substPtr :: Var -> Ptr -> Term -> Term
substPtr x ptr = go
  where
    go :: Term -> Term
    go (TVar x')    = if x == x' then TPtr ptr else TVar x'
    go (TApp e1 e2) = TApp (go e1) (go e2)
    go (TLam x' e)  = TLam x' $ if x == x' then e else go e
    go (TPtr ptr')  = TPtr ptr'
    go (TCon c es)  = TCon c (map go es)
    go (TPat e ms)  = TPat (go e) (map goM ms)

    goM :: Match -> Match
    goM (Match (Pat c xs) e) = Match (Pat c xs) $ if x `elem` xs then e else go e

allocSubst :: [(Var, Term)] -> (Heap Term, Term) -> (Heap Term, Term)
allocSubst []          (hp, e) = (hp, e)
allocSubst ((x, s):ss) (hp, e) =
    case s of
      TPtr ptr   -> allocSubst ss (hp, substPtr x ptr e)
      _otherwise -> let (hp', ptr) = alloc hp s
                        e'         = substPtr x ptr e
                    in allocSubst ss (hp', e')
