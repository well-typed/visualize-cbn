module CBN.Subst (
    subst
  , substVar
  , substPtr
  , substVars
  , substPtrs
  , allocSubst
  ) where

import Data.Bifunctor
import Data.List (partition)
import Data.Map (Map)

import qualified Data.Map as Map

import CBN.Free
import CBN.Heap
import CBN.Language

{-------------------------------------------------------------------------------
  Substitution
-------------------------------------------------------------------------------}

-- | Substitution
--
-- NOTE: Although we deal with shadowing here @(\x -> .. (\x -> .. ))@, we
-- do NOT implement capture avoiding substitution. Since we never reduce
-- under binders, we can never have free variables, and hence this is not
-- something we need to worry about.
class Subst a where
  subst :: Either Ptr Var -> Term -> a -> a

substVar :: Subst a => Var -> Term -> a -> a
substVar = subst . Right

substPtr :: Subst a => Ptr -> Term -> a -> a
substPtr = subst . Left

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

instance Subst a => Subst [a] where
  subst x e = map (subst x e)

instance Subst Term where
  subst x e term =
      case term of
        TPtr x'       -> if x == Left  x' then e else term
        TVar x'       -> if x == Right x' then e else term
        TLam x' e1    -> if x == Right x'
                           then term
                           else TLam x' (subst x e e1)
        TLet bound e' -> if x `elem` map (Right . fst) bound
                           then term
                           else TLet (map (second (subst x e)) bound)
                                     (subst x e e')
        TCon ces      -> TCon  (subst x e ces)
        TPrim pes     -> TPrim (subst x e pes)
        TApp e1 e2    -> TApp  (subst x e e1) (subst x e e2)
        TCase e1 ms   -> TCase (subst x e e1) (subst x e ms)
        TSeq e1 e2    -> TSeq  (subst x e e1) (subst x e e2)
        TIf c t f     -> TIf   (subst x e c)  (subst x e t)  (subst x e f)

instance Subst ConApp where
  subst x e (ConApp c es) = ConApp c (subst x e es)

instance Subst PrimApp where
  subst x e (PrimApp p es) = PrimApp p (subst x e es)

instance Subst Match where
  subst x e (Match (Pat c xs) e') =
    if x `elem` map Right xs
      then Match (Pat c xs)            e'
      else Match (Pat c xs) (subst x e e')

instance Subst Branches where
  subst x e (Matches ms) = Matches (map (subst x e) ms)
  subst x e (Selector s) = Selector (subst x e s)

instance Subst Selector where
  subst _ _ = id

{-------------------------------------------------------------------------------
  Many-variable substitution
-------------------------------------------------------------------------------}

substMany :: Subst a => [(Either Ptr Var, Term)] -> a -> a
substMany []         = id
substMany ((x, e):s) = substMany (map (second (subst x e)) s) . subst x e

substVars :: Subst a => [(Var, Term)] -> a -> a
substVars = substMany . map (first Right)

substPtrs :: Subst a => [(Ptr, Term)] -> a -> a
substPtrs = substMany . map (first Left)

{-------------------------------------------------------------------------------
  Heap allocation
-------------------------------------------------------------------------------}

allocSubst :: [(Var, Term)] -> (Heap Term, Term) -> (Heap Term, Term)
allocSubst bindings (heap, body) =
    let toAlloc, toSubst :: [(Var, Term)]
        (toAlloc, toSubst) = partition requiresAlloc bindings

        body' :: Term
        body' = substVars toSubst body

        heap'      :: Heap Term
        substAlloc :: [(Var, Term)]
        (heap', substAlloc) =
            allocMany
              (map prepareHeapEntry $ map (second (substVars toSubst)) toAlloc)
              processHeapEntries
              heap

    in (heap', substVars substAlloc body')
  where
    -- We all all post-processing in 'processHeapEntries'
    prepareHeapEntry :: (Var, Term) -> (Maybe String, Ptr -> (Var, Term, Ptr))
    prepareHeapEntry (x, t) = (
          Just (varName x)
        , \ptr -> (x, t, ptr)
        )

    -- New heap entries, along with substitution for all heap-allocated vars
    processHeapEntries :: [(Var, Term, Ptr)] -> ([(Ptr, Term)], [(Var, Term)])
    processHeapEntries entries = (
          map (\(_, t, ptr) -> (ptr, substVars substAlloc t)) entries
        , substAlloc
        )
      where
        substAlloc :: [(Var, Term)]
        substAlloc = map (\(x, _, ptr) ->  (x, TPtr ptr)) entries

    -- Do we need to allocate this term?
    requiresAlloc :: (Var, Term) -> Bool
    requiresAlloc (x, t) = and [
          not $ termIsSimple t
        , not $ isUsedOnceInBody x
        ]

    -- Is this binding used only once, and only in the body?
    isUsedOnceInBody :: Var -> Bool
    isUsedOnceInBody x = and [
          x `notElem` Map.keys freeInBindings
        , Map.findWithDefault 0 x freeInBody <= 1
        ]
      where
        freeInBindings, freeInBody :: Map Var Count
        freeInBindings = free $ map snd bindings
        freeInBody     = free body






