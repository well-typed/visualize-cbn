module CBN.Trace (
    -- * Traces
    Trace(..)
  , TraceCont(..)
  , traceTerm
    -- * Summarizing
  , SummarizeOptions(..)
  , summarize
  ) where

import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set

import CBN.Eval
import CBN.Free
import CBN.Heap
import CBN.Language

{-------------------------------------------------------------------------------
  Constructing the trace
-------------------------------------------------------------------------------}

data Trace = Trace (Heap Term, Term) TraceCont

data TraceCont =
    -- | The trace finished on a weak head normal form
    TraceWHNF Value

    -- | The trace got stuck
  | TraceStuck Error

    -- | The trace was stopped because the maximum number of steps was reached
  | TraceStopped

    -- | We took one reduction step
  | TraceStep Description Trace

    -- | The garbage collector removed some pointers
  | TraceGC (Set Ptr) Trace

traceTerm :: Bool -> (Heap Term, Term) -> Trace
traceTerm shouldGC = go
  where
    go :: (Heap Term, Term) -> Trace
    go (hp, e) = Trace (hp, e) $
      case step (hp, e) of
        WHNF val         -> TraceWHNF  val
        Stuck err        -> TraceStuck err
        Step d (hp', e') -> TraceStep d $
          if shouldGC
            then let (hp'', collected) = gc e' hp'
                 in if Set.null collected
                      then go (hp'', e')
                      else Trace (hp', e') $ TraceGC collected $ go (hp'', e')
            else go (hp', e')

    gc :: Term -> Heap Term -> (Heap Term, Set Ptr)
    gc = markAndSweep . pointers

{-------------------------------------------------------------------------------
  Summarizing traces
-------------------------------------------------------------------------------}

data SummarizeOptions = SummarizeOptions {
      summarizeCollapseBeta :: Bool
    , summarizeMaxNumSteps  :: Int
    , summarizeHidePrelude  :: Bool
    , summarizeHideGC       :: Bool
    }
  deriving (Show)

summarize :: SummarizeOptions -> Trace -> Trace
summarize SummarizeOptions{..} = go 0
  where
    go :: Int -> Trace -> Trace
    go n (Trace (hp, e) c) = Trace (goHeap hp, e) $ goCont n c

    goCont :: Int -> TraceCont -> TraceCont
    goCont _ (TraceWHNF v)    = TraceWHNF v
    goCont _ (TraceStuck err) = TraceStuck err
    goCont _ TraceStopped     = TraceStopped
    goCont n (TraceGC ps t'@(Trace _ c')) =
      if summarizeHideGC
        then goCont (n + 1) c'
        else TraceGC ps $ go (n + 1) t'
    goCont n (TraceStep d t) =
      case d of
        _ | n > summarizeMaxNumSteps ->
          TraceStopped
        StepApply _ | summarizeCollapseBeta ->
          TraceStep d $ goBeta (n + 1) t
        StepBeta | summarizeCollapseBeta ->
          TraceStep d $ goBeta (n + 1) t
        _otherwise ->
          TraceStep d $ go     (n + 1) t

    -- | We already saw one beta reduction; skip any subsequent ones
    goBeta :: Int -> Trace -> Trace
    goBeta n t@(Trace _ c) = case c of
      TraceStep StepBeta t' -> goBeta (n + 1) t'
      _otherwise            -> go     (n + 1) t

    -- | Cleanup the heap
    goHeap :: Heap Term -> Heap Term
    goHeap (Heap next heap) = Heap next $
      if not summarizeHidePrelude
        then heap
        else Map.filterWithKey (\ptr -> not . isPrelude ptr) heap

    -- | Does this entry in the heap come from the prelude?
    isPrelude :: Ptr -> Term -> Bool
    isPrelude (Ptr Nothing (Just _)) _ = True
    isPrelude _ _ = False
