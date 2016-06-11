module CBN.Trace (
    Trace(..)
  , TraceCont(..)
  , trace
  , limitSteps
  ) where

import CBN.Eval
import CBN.Heap
import CBN.Language

data Trace = Trace (Heap Term, Term) TraceCont
data TraceCont = TraceWHNF Value
               | TraceStuck Error
               | TraceStopped
               | TraceStep Trace

trace :: (Heap Term, Term) -> Trace
trace (hp, e) = Trace (hp, e) $
    case step hp e of
      WHNF val    -> TraceWHNF   val
      Stuck err   -> TraceStuck  err
      Step hp' e' -> TraceStep $ trace (hp', e')

limitSteps :: Int -> Trace -> Trace
limitSteps 0 (Trace (hp, e) _) = Trace (hp, e) TraceStopped
limitSteps n (Trace (hp, e) c) = Trace (hp, e) $
  case c of
    TraceStep t -> TraceStep $ limitSteps (n-1) t
    _otherwise  -> c
