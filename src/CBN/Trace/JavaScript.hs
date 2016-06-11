module CBN.Trace.JavaScript (render) where

import Text.Blaze.Html.Renderer.String

import CBN.Eval
import CBN.Pretty.HTML
import CBN.Trace

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
         "if(frame == " ++ show n ++ ") {\n"
      ++ set "heap" (renderHtml (toHtml hp))
      ++ set "term" (renderHtml (toHtml e))
      ++ case c of
           TraceWHNF _     -> set "status" "whnf"      ++ "}\n"
           TraceStuck err  -> set "status" (mkErr err) ++ "}\n"
           TraceStopped    -> set "status" "stopped"   ++ "}\n"
           TraceStep d tr' -> set "status" (mkDesc d)  ++ "}\n" ++ go (n + 1) tr'

    mkErr :: String -> String
    mkErr = ("error: " ++)

    mkDesc :: Description -> String
    mkDesc d = "next step: " ++ renderHtml (toHtml d)

    set :: String -> String -> String
    set suffix val = innerHTML suffix ++ " = " ++ show val ++ ";\n"

    innerHTML :: String -> String
    innerHTML suffix = "document.getElementById(\"" ++ name ++ "_" ++ suffix ++ "\").innerHTML"
