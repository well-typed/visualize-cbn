module CBN.Trace.Textual (visualize) where

import CBN.Trace
import CBN.Pretty

visualize :: Trace -> String
visualize = go 0
  where
    go :: Int -> Trace -> String
    go n (Trace (hp, e) c) =
         "** " ++ show n ++ "\n"
      ++ show (pretty hp) ++ "\n"
      ++ show (pretty e)  ++ "\n"
      ++ "\n"
      ++ case c of
           TraceWHNF _    -> "(whnf)"
           TraceStuck err -> "(stuck: " ++ err ++ ")"
           TraceStopped   -> "(stopped)"
           TraceStep tr'  -> go (n + 1) tr'
