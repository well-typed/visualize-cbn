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
class Subst a where
  subst :: Var -> Term -> a -> a

instance Subst a => Subst [a] where
  subst x e = map (subst x e)

instance Subst Term where
  subst _ _ (TPtr ptr')     = TPtr ptr'
  subst x e (TVar x')       = if x == x' then e
                                         else TVar x'
  subst x e (TLam x' e1)    = if x == x' then TLam x'             e1
                                         else TLam x' (subst x e e1)
  subst x e (TLet x' e1 e2) = if x == x' then TLet x'             e1            e2
                                         else TLet x' (subst x e e1) (subst x e e2)
  subst x e (TCon ces)      = TCon  (subst x e ces)
  subst x e (TPrim pes)     = TPrim (subst x e pes)
  subst x e (TApp e1 e2)    = TApp  (subst x e e1) (subst x e e2)
  subst x e (TCase e1 ms)   = TCase (subst x e e1) (subst x e ms)
  subst x e (TSeq e1 e2)    = TSeq  (subst x e e1) (subst x e e2)
  subst x e (TIf c t f)     = TIf   (subst x e c)  (subst x e t)  (subst x e f)

instance Subst ConApp where
  subst x e (ConApp c es) = ConApp c (subst x e es)

instance Subst PrimApp where
  subst x e (PrimApp p es) = PrimApp p (subst x e es)

instance Subst Match where
  subst x e (Match (Pat c xs) e') =
    if x `elem` xs then Match (Pat c xs)            e'
                   else Match (Pat c xs) (subst x e e')

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
    isSimple (TPtr _)               = True
    isSimple (TCon (ConApp _ []))   = True
    isSimple (TPrim (PrimApp _ [])) = True
    isSimple _                      = False

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
