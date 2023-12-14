{-------------------------------------------------------------------------------
  Selector (thunk) optimization

  References:

  - "Fixing some space leaks with a garbage collector", Philip Walder
    <https://homepages.inf.ed.ac.uk/wadler/topics/garbage-collection.html>

  - "A Concurrent Garbage Collector for the Glasgow Haskell Compiler", Ben Gamari
    <https://well-typed.com/blog/aux/files/nonmoving-gc/design.pdf>
    Specifically section 2.5.7, "Selector optimization"

  - "Three runtime optimizations done by GHC's GC", Ömer Sinan Ağacan
    Specifically section 3, "Selector thunk evaluation"

  - "GHC Commentary: The Layout of Heap Objects", section "Selector thunks"
    <https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/rts/storage/heap-objects#selector-thunks>
-------------------------------------------------------------------------------}

module CBN.SelThunkOpt (selThunkOpt) where

import Control.Applicative
import Control.Monad.State
import Data.Set (Set)

import qualified Data.Map as Map
import qualified Data.Set as Set

import CBN.Eval
import CBN.Heap
import CBN.Language

-- | Apply selector thunk optimization
selThunkOpt :: Heap Term -> (Heap Term, Set Ptr)
selThunkOpt = findAll Set.empty
  where
    findAll :: Set Ptr -> Heap Term -> (Heap Term, Set Ptr)
    findAll acc hp =
        case asum $ map (findOne hp) (Map.toList $ heapEntries hp) of
          Nothing         -> (hp, acc)
          Just (ptr, hp') -> findAll (Set.insert ptr acc) hp'

    -- Find one term to step
    findOne :: Heap Term -> (Ptr, Term) -> Maybe (Ptr, Heap Term)
    findOne hp (ptr, e) = do
        (hp', e') <- applyInTerm hp e
        return (ptr, mutate (hp', ptr) e')

-- | Apply selector-thunk optimization in this term
--
-- Returns 'Nothing' if there were no opportunities to apply the optimization.
applyInTerm :: Heap Term -> Term -> Maybe (Heap Term, Term)
applyInTerm = \hp term -> do
    let (term', (hp', isChanged)) = runState (go term) (hp, False)
    guard isChanged
    return (hp', term')
  where
    go :: Term -> State (Heap Term, Bool) Term

    -- Term that cannot change

    go term@TVar{} = return term
    go term@TLam{} = return term -- We don't look inside binders
    go term@TPtr{} = return term

    -- Propagation

    go (TCon (ConApp con args)) =
        TCon . ConApp con <$> mapM go args
    go (TPrim (PrimApp prim args)) =
        TPrim . PrimApp prim <$> mapM go args
    go (TLet bound e) =
        TLet <$> mapM (\(x, t) -> (x,) <$> go t) bound <*> go e
    go (TApp e1 e2) =
        TApp <$> go e1 <*> go e2
    go (TIf c t f) =
        TIf <$> go c <*> go t <*> go f
    go (TSeq e1 e2) =
        TSeq <$> go e1 <*> go e2
    go (TCase e (Matches ms)) =
        TCase <$> go e <*> (Matches <$> mapM goMatch ms)
      where
        goMatch :: Match -> State (Heap Term, Bool) Match
        goMatch (Match pat rhs) = Match pat <$> go rhs

    -- The interesting case
    --
    -- This code is a bit simpler than the corresponding code in evaluation,
    -- because we /only/ deal with selectors, not general case statements. This
    -- means we don't need to care about substitution, but can literally just
    -- select the right argument (using

    go term@(TCase e (Selector s)) = do
        (hp, _) <- get
        mConApp <-
          case allocConArgs (selectorVars s) (hp, e) of
            ConArgsAllocFailed ->
              return Nothing
            ConArgsAllocUnnecessary conApp ->
              return $ Just conApp
            ConArgsAllocDone (_ctxt, hp', _e') conApp -> do
              put (hp', True)
              return $ Just conApp
        case mConApp of
          Just (ConApp con args) | con == selectorCon s -> do
            modify $ \(hp', _) -> (hp', True)
            return $ args !! selectorIndex s
          _otherwise ->
            return term

