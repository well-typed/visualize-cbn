module CBN.Trace.JavaScript (render) where

import Data.Set (Set)
import Text.Blaze.Html.Renderer.String
import Text.Blaze.Html5 (toHtml)
import qualified Data.Set as Set

import CBN.Eval
import CBN.Heap
import CBN.Pretty
import CBN.Trace
import CBN.Util.Doc.Rendered.HTML ()
import qualified CBN.Util.Doc          as Doc
import qualified CBN.Util.Doc.Rendered as Rendered

render :: String -> Trace -> String
render name = \tr ->
       "function " ++ name ++ "(frame) {\n"
    ++ innerHTML "step" ++ " = frame;\n"
    ++ go 0 tr
    ++ "}\n"
    ++ "var " ++ name ++ "_frame = 0;\n"
    ++ "function " ++ name ++ "Next() {\n"
    ++ name ++ "(++" ++ name ++ "_frame" ++ ");\n"
    ++ "}\n"
    ++ "function " ++ name ++ "Prev() {\n"
    ++ name ++ "(--" ++ name ++ "_frame" ++ ");\n"
    ++ "}\n"
    ++ name ++ "(" ++ name ++ "_frame);\n"
  where
    go :: Int -> Trace -> String
    go n (Trace (hp, e) c) =
        case c of
          TraceWHNF _     -> mkFrame Set.empty "whnf"
          TraceStuck err  -> mkFrame Set.empty (mkErr err)
          TraceStopped    -> mkFrame Set.empty "stopped"
          TraceStep d tr' -> mkFrame Set.empty (mkDesc d) ++ go (n + 1) tr'
          TraceGC  ps tr' -> mkFrame ps        "gc"       ++ go (n + 1) tr'
      where
        mkFrame :: Set Ptr -> String -> String
        mkFrame garbage status =
             "if(frame == " ++ show n ++ ") {\n"
          ++ set "heap" (pretty (heapToDoc garbage hp))
          ++ set "term" (pretty e)
          ++ set "status" status
          ++ "}\n"

    mkErr :: String -> String
    mkErr = ("error: " ++)

    mkDesc :: Description -> String
    mkDesc d = "next step: " ++ pretty d

    set :: String -> String -> String
    set suffix val = innerHTML suffix ++ " = " ++ show val ++ ";\n"

    innerHTML :: String -> String
    innerHTML suffix = "document.getElementById(\"" ++ name ++ "_" ++ suffix ++ "\").innerHTML"

    pretty :: ToDoc a => a -> String
    pretty = renderHtml
           . toHtml
           . Doc.render (\r -> Rendered.width r <= 80)
           . toDoc
