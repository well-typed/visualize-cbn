module CBN.Trace.Textual (render) where

import CBN.Trace
import CBN.Pretty.Doc

render :: Trace -> String
render = go 0
  where
    go :: Int -> Trace -> String
    go n (Trace (hp, e) c) =
         "** " ++ show n ++ "\n"
      ++ show (pretty hp) ++ "\n"
      ++ show (pretty e)  ++ "\n"
      ++ "\n"
      ++ case c of
           TraceWHNF _      -> "(whnf)"
           TraceStuck err   -> "(stuck: " ++ err ++ ")"
           TraceStopped     -> "(stopped)"
           TraceStep _d tr' -> go (n + 1) tr'
