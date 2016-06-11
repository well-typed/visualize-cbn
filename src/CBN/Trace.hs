module CBN.Trace (
    Trace(..)
  , TraceCont(..)
  , traceTerm
  , limitSteps
  ) where

import CBN.Eval
import CBN.Heap
import CBN.Language

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

limitSteps :: Int -> Trace -> Trace
limitSteps 0 (Trace (hp, e) _) = Trace (hp, e) TraceStopped
limitSteps n (Trace (hp, e) c) = Trace (hp, e) $
  case c of
    TraceStep d t -> TraceStep d $ limitSteps (n-1) t
    _otherwise    -> c
