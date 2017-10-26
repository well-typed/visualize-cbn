module CBN.Trace.Graph (render) where

import CBN.Trace

render :: Trace -> String
render (Trace (ht, t) cont) =
  "digraph G {\n"
  ++ "a0 -> a1;\n"
  ++ "}"
