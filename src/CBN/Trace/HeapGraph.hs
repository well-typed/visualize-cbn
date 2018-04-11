{-# LANGUAGE OverloadedStrings #-}

-- This module was inspired by CBN.Trace.Graph and has a lot of code repetition.

module CBN.Trace.HeapGraph (toGraphFiles) where

import Control.Monad
import Data.Graph (Graph)
import Data.Monoid ((<>))
import qualified Data.Graph as Graph
import qualified Data.Text as T

import CBN.Closure
import CBN.Pretty
import CBN.Trace
import CBN.Util.Doc.Style
import qualified CBN.Util.Doc          as Doc
import qualified CBN.Util.Doc.Rendered as Rendered

toGraphFiles :: Trace -> FilePath -> IO ()
toGraphFiles trace pathAndPrefix = forM_ (renderMemoryTrace trace) $
  \(k,v) -> writeFile (pathAndPrefix ++ show k ++ ".dot") v

renderMemoryTrace :: Trace -> [(Int,String)]
renderMemoryTrace = go 0
  where
    go n (Trace (hp, t) cont) = (n,x):xs
      where
        x = renderMemoryGraph $ toClosureGraph (hp, t)
        xs = case cont of
          TraceStep _ tr' -> go (n + 1) tr'
          TraceGC _ tr'  -> go (n + 1) tr'
          _               -> []

renderMemoryGraph :: (Graph, Graph.Vertex ->
 (Closure, Id, [Id]), Id -> Graph.Vertex) -> String
renderMemoryGraph (graph, f, g) =
  "digraph G {\n"
  ++ "node [ fontname=monospace, shape=plaintext ];\n"
  ++ concatMap mkFrame (Graph.vertices graph)
  ++ "}"
  where
    mkFrame :: Graph.Vertex -> String
    mkFrame vertex =
      let (closure, _, ids) = f vertex
          rows :: T.Text
          rows = mkRow (pretty closure)
      in T.unpack $
        setLabel vertex ("<<TABLE ALIGN=\"LEFT\">" <> rows <> "</TABLE>>")
        <> "\n"
        <> mkConnections vertex (map g ids)

    mkRow :: T.Text -> T.Text
    mkRow content = "<TR><TD BALIGN=\"LEFT\" ALIGN=\"LEFT\">" <> content <> "</TD></TR>"

    setLabel :: Graph.Vertex -> T.Text -> T.Text
    setLabel n label = mkNode n <> "[label=" <> label <> "];"

    mkConnections :: Graph.Vertex -> [Graph.Vertex] -> T.Text
    mkConnections n [] = mkNode n <> ";\n"
    mkConnections n adj = mkNode n <> " -> " <> T.intercalate ", " (map mkNode adj) <> ";\n"

    mkNode :: Graph.Vertex -> T.Text
    mkNode n = "s" <> T.pack (show n)

    pretty :: ToDoc a => a -> T.Text
    pretty = T.pack . goRendered . Rendered.rendered . Doc.render (\r -> Rendered.width r <= 80) . toDoc

    goRendered :: [[Maybe (Style, Char)]] -> String
    goRendered []       = ""
    goRendered (row:xs) = goRow row ++ "<BR />" ++ goRendered xs

    goRow :: [Maybe (Style, Char)] -> String
    goRow = mconcat . map toDotHtml . groupByStyle

    toDotHtml :: (Style, String) -> String
    toDotHtml (Style Nothing _ True _, str) = "<B>" <> escapeChars str <> "</B>"
    toDotHtml (Style Nothing _ _ True, str) = "<I>" <> escapeChars str <> "</I>"
    toDotHtml (Style (Just fg) _ _ _, str) =
      let color = case fg of Blue -> "blue"; Red -> "red"
      in
        "<FONT COLOR=\"" <> color <> "\">" <> escapeChars str <> "</FONT>"
    toDotHtml (Style Nothing _ False False, str) = escapeChars str

    escapeChars :: String -> String
    escapeChars =
      T.unpack
      . T.replace "\n" "<BR />"
      . T.replace ">" "&gt;"
      . T.replace "<" "&lt;"
      . T.replace " " "&nbsp;"
      . T.pack
