module CBN.Trace.JavaScript (render) where

import Data.Maybe (listToMaybe)
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

render :: String -> Maybe FilePath -> Trace -> String
render name graph = \tr ->
       "function " ++ name ++ "(frame) {\n"
    ++ innerHTML "step" ++ " = frame;\n"
    ++ mkGraph
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
    mkGraph :: String
    mkGraph = case graph of
      Nothing -> ""
      Just g  ->
        innerHTML "graph" ++ " = \'<img src=\\'"
        ++ g ++ "' + frame.toString() + '.png\\'>';\n"

    go :: Int -> Trace -> String
    go n (Trace (hp, e) c) =
        case c of
          TraceWHNF _          -> mkFrame Set.empty Nothing "whnf"
          TraceStuck err       -> mkFrame Set.empty Nothing (mkErr err)
          TraceStopped         -> mkFrame Set.empty Nothing "stopped"
          TraceStep d tr'      -> mkFrame Set.empty (mkFocus d) (mkDesc d) ++ go (n + 1) tr'
          TraceGC ps tr'       -> mkFrame ps Nothing "gc"       ++ go (n + 1) tr'
          TraceSelThunk ps tr' -> mkFrame ps Nothing "selector" ++ go (n + 1) tr'
          TraceInline ps tr'   -> mkFrame ps Nothing "inline"   ++ go (n + 1) tr'
      where
        mkFrame :: Set Ptr -> Maybe Ptr -> String -> String
        mkFrame garbage focus status =
             "if(frame == " ++ show n ++ ") {\n"
          ++ set "heap" (pretty (heapToDoc garbage focus hp))
          ++ set "term" (pretty e)
          ++ set "status" status
          ++ "}\n"

    mkErr :: String -> String
    mkErr = ("error: " ++)

    mkDesc :: DescriptionWithContext -> String
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

    mkFocus :: DescriptionWithContext -> Maybe Ptr
    mkFocus (DescriptionWithContext _ ctxt) = listToMaybe (reverse ctxt)
