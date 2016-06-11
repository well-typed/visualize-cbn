module CBN.Trace (
    -- * Traces
    Trace(..)
  , TraceCont(..)
  , traceTerm
    -- * Summarizing
  , SummarizeOptions(..)
  , summarize
  ) where

import qualified Data.Map as Map

import CBN.Eval
import CBN.Heap
import CBN.Language

{-------------------------------------------------------------------------------
  Constructing the trace
-------------------------------------------------------------------------------}

data Trace = Trace (Heap Term, Term) TraceCont
data TraceCont = TraceWHNF Value
               | TraceStuck Error
               | TraceStopped
               | TraceStep Description Trace

traceTerm :: (Heap Term, Term) -> Trace
traceTerm (hp, e) = Trace (hp, e) $
    case step (hp, e) of
      WHNF val         -> TraceWHNF  val
      Stuck err        -> TraceStuck err
      Step d (hp', e') -> TraceStep d $ traceTerm (hp', e')

{-------------------------------------------------------------------------------
  Summarizing traces
-------------------------------------------------------------------------------}

data SummarizeOptions = SummarizeOptions {
      summarizeAdjacentBeta :: Bool
    , summarizeMaxNumSteps  :: Int
    , summarizeHidePrelude  :: Bool
    }
  deriving (Show)

summarize :: SummarizeOptions -> Trace -> Trace
summarize SummarizeOptions{..} = go 0
  where
    go :: Int -> Trace -> Trace
    go n (Trace (hp, e) c) = Trace (goHeap hp, e) $
      case c of
        TraceWHNF v    -> TraceWHNF v
        TraceStuck err -> TraceStuck err
        TraceStopped   -> TraceStopped
        TraceStep d t  -> case d of
          _ | n > summarizeMaxNumSteps        -> TraceStopped
          StepApply _ | summarizeAdjacentBeta -> TraceStep d $ goBeta (n + 1) t
          StepBeta    | summarizeAdjacentBeta -> TraceStep d $ goBeta (n + 1) t
          _otherwise                          -> TraceStep d $ go     (n + 1) t

    -- | We already saw one beta reduction; skip any subsequent ones
    goBeta :: Int -> Trace -> Trace
    goBeta n t@(Trace _ c) = case c of
      TraceStep StepBeta t' -> goBeta (n + 1) t'
      _otherwise            -> go     (n + 1) t

    -- | Cleanup the heap
    goHeap :: Heap Term -> Heap Term
    goHeap (Heap heap) = Heap $
      if not summarizeHidePrelude
        then heap
        else Map.filterWithKey (\ptr -> not . isPrelude ptr) heap

    -- | Does this entry in the heap come from the prelude?
    isPrelude :: Ptr -> Term -> Bool
    isPrelude (Ptr Nothing (Just _)) _ = True
    isPrelude _ _ = False
