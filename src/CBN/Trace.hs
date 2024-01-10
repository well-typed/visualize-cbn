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
import CBN.InlineHeap
import CBN.Language
import CBN.SelThunkOpt

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
  | TraceStep DescriptionWithContext Trace

    -- | The garbage collector removed some pointers
  | TraceGC (Set Ptr) Trace

    -- | The selector thunk optimization was applied
    --
    -- We separately record if the selector thunk was applied at the top-level.
  | TraceSelThunk Bool (Set Ptr) Trace

    -- | We simplified the heap by inlining some definitions
  | TraceInline (Set Ptr) Trace

traceTerm :: Bool -> Bool -> Bool -> (Heap Term, Term) -> Trace
traceTerm shouldGC shouldInline enableSelThunkOpt = go
  where
    go :: (Heap Term, Term) -> Trace
    go (hp, e) = Trace (hp, e) $
      case step (hp, e) of
        WHNF val         -> TraceWHNF  val
        Stuck err        -> TraceStuck err
        Step d (hp1, e1) ->
          let (traceSelThunkOpt, hp2, e2)
                | enableSelThunkOpt
                = let (hp', e', atToplevel, optimized) = selThunkOpt hp1 e1
                  in if not atToplevel && Set.null optimized then
                       (id, hp1, e1)
                     else
                       (Trace (hp1, e1) . TraceSelThunk atToplevel optimized, hp', e')
                | otherwise
                = (id, hp1, e1) in

          let (traceGC, hp3, e3)
                 | shouldGC
                 = let (hp', collected) = gc e2 hp2
                   in if Set.null collected then
                        (id, hp2, e2)
                      else
                        (Trace (hp2, e2) . TraceGC collected, hp', e2)

                 | otherwise
                 = (id, hp2, e2) in

          let (traceInlining, hp4, e4)
                | shouldInline
                = let (hp', e', inlined) = inlineHeap hp3 e3
                  in if Set.null inlined then
                       (id, hp3, e3)
                     else
                       (Trace (hp3, e3) . TraceInline inlined, hp', e')

                | otherwise
                = (id, hp3, e3) in

          TraceStep d
            $ traceSelThunkOpt . traceGC . traceInlining
            $ go (hp4, e4)

    gc :: Term -> Heap Term -> (Heap Term, Set Ptr)
    gc = markAndSweep . pointers

{-------------------------------------------------------------------------------
  Summarizing traces
-------------------------------------------------------------------------------}

data SummarizeOptions = SummarizeOptions {
      summarizeCollapseBeta :: Bool
    , summarizeMaxNumSteps  :: Int
    , summarizeHidePrelude  :: Maybe Int
    , summarizeHideTerms    :: [String]
    , summarizeHideGC       :: Bool
    , summarizeHideSelThunk :: Bool
    , summarizeHideInlining :: Bool
    }
  deriving (Show)

summarize :: SummarizeOptions -> Trace -> Trace
summarize SummarizeOptions{..} = go 0
  where
    -- If we have
    --
    -- >    step1     step2
    -- > x ------> y ------> z
    --
    -- and we want to hide step2 (say, GC), then we want to get
    --
    -- > x  step1
    -- > x -------> z
    --
    -- We will realize we want to hide this step when we look at @step2@; this
    -- means that we may want to hide the /source/ of the step (@y@), and
    -- instead show the destination (@z@).
    go :: Int -> Trace -> Trace
    go n (Trace (hp, e) c) =
        case c of
          -- End of the trace

          TraceWHNF v    -> showSrc $ TraceWHNF v
          TraceStuck err -> showSrc $ TraceStuck err
          TraceStopped   -> showSrc $ TraceStopped
          TraceStep{}
            | n > summarizeMaxNumSteps
                         -> showSrc $ TraceStopped


          -- Potential hiding steps

          TraceGC ps t' ->
            if summarizeHideGC
              then go (n + 1) t'
              else showSrc $ TraceGC ps $ go (n + 1) t'
          TraceSelThunk atToplevel ps t' ->
            if summarizeHideSelThunk
              then go (n + 1) t'
              else showSrc $ TraceSelThunk atToplevel ps $ go (n + 1) t'
          TraceInline ps t' ->
            if summarizeHideInlining
              then go (n + 1) t'
              else showSrc $ TraceInline ps $ go (n + 1) t'

          -- Collapsing multiple beta-reductions
          --
          -- This is a little different because we don't want to hide the
          -- step from the trace entirely; we just want to collapse multiple
          -- steps into one, but still marking that as a beta step.

          TraceStep dwc t' ->
            if summarizeCollapseBeta && isBetaStep dwc
              then Trace (hp, e) $ goBeta (n + 1) t'
              else showSrc $ TraceStep dwc $ go (n + 1) t'

      where
        showSrc :: TraceCont -> Trace
        showSrc = Trace (goHeap n hp, e)

    -- | We already saw one beta reduction; skip any subsequent ones
    goBeta :: Int -> Trace -> TraceCont
    goBeta n t@(Trace _ c) =
        case c of
          TraceStep dwc t' | isBetaStep dwc ->
            goBeta (n + 1) t'
          _otherwise ->
            TraceStep (DescriptionWithContext StepBeta []) $ go n t

    isBetaStep :: DescriptionWithContext -> Bool
    isBetaStep (DescriptionWithContext d _ctxt) =
        case d of
          StepBeta    -> True
          StepApply{} -> True
          _otherwise  -> False

    -- | Cleanup the heap
    goHeap :: Int -> Heap Term -> Heap Term
    goHeap n (Heap next heap) =
        Heap next $ Map.filterWithKey shouldShow heap
      where
        shouldShow :: Ptr -> Term -> Bool
        shouldShow (Ptr Nothing (Just name)) _ = and [
              case summarizeHidePrelude of
                Nothing -> True
                Just n' -> n < n'
            , not (name `elem` summarizeHideTerms)
            ]
        shouldShow (Ptr _ _) _ = True


