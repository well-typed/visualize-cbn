module CBN.Trace.Textual (render) where

import Data.List (intercalate)
import Data.Set (Set)
import qualified Data.Set as Set

import CBN.Heap
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
           TraceStep d  tr' -> "(" ++ show (pretty d)   ++ ")\n" ++ go (n + 1) tr'
           TraceGC   ps tr' -> "(collect " ++ goPtrs ps ++ ")\n" ++ go (n + 1) tr'


    goPtrs :: Set Ptr -> String
    goPtrs = intercalate "," . map (show . pretty) . Set.toList
