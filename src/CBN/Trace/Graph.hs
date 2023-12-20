{-# LANGUAGE OverloadedStrings #-}

module CBN.Trace.Graph (render) where

import Data.Set (Set)
import Data.Maybe (listToMaybe)

import qualified Data.Set  as Set
import qualified Data.Text as T

import CBN.Eval
import CBN.Heap
import CBN.Pretty
import CBN.Trace
import CBN.Util.Doc.Style

import qualified CBN.Util.Doc          as Doc
import qualified CBN.Util.Doc.Rendered as Rendered

render :: Trace -> String
render tr =
  "digraph G {\n"
  ++ "node [ fontname=monospace, shape=plaintext ];\n"
  ++ go 0 tr
  ++ "}"
  where
    go :: Int -> Trace -> String
    go index (Trace (hp, t) cont) =
      case cont of
        TraceWHNF _          -> mkFrame Set.empty Nothing "whnf"
        TraceStuck err       -> mkFrame Set.empty Nothing (mkErr err)
        TraceStopped         -> mkFrame Set.empty Nothing "stopped"
        TraceStep d tr'      -> mkFrame Set.empty (mkFocus d) (mkDesc d) ++ go (index + 1) tr'
        TraceGC ps tr'       -> mkFrame ps Nothing "gc"       ++ go (index + 1) tr'
        TraceSelThunk ps tr' -> mkFrame ps Nothing "selector" ++ go (index + 1) tr'
        TraceInline ps tr'   -> mkFrame ps Nothing "inline"   ++ go (index + 1) tr'
      where
        mkFrame :: Set Ptr -> Maybe Ptr -> T.Text -> String
        mkFrame garbage focus status =
          T.unpack $
            setLabel index ("<<TABLE ALIGN=\"LEFT\">" <> rows <> "</TABLE>>")
            <> "\n"
            <> mkConnection index
          where
            rows :: T.Text
            rows = mkRow (pretty t)
                <> mkRow (pretty (heapToDoc garbage focus hp))
                <> mkRow status

        mkRow :: T.Text -> T.Text
        mkRow content = "<TR><TD BALIGN=\"LEFT\" ALIGN=\"LEFT\">" <> content <> "</TD></TR>"

        escapeChars :: String -> String
        escapeChars =
          T.unpack
          . T.replace "\n" "<BR />"
          . T.replace ">" "&gt;"
          . T.replace "<" "&lt;"
          . T.replace " " "&nbsp;"
          . T.pack

        setLabel :: Int -> T.Text -> T.Text
        setLabel n label = mkNode n <> "[label=" <> label <> "];"

        mkConnection :: Int -> T.Text
        mkConnection n
          | n == 0 = mkNode 0 <> ";\n"
          | otherwise = mkNode (n - 1) <> " -> " <> mkNode n <> ";\n"

        mkNode :: Int -> T.Text
        mkNode n = "s" <> T.pack (show n)

        mkErr :: String -> T.Text
        mkErr = ("error: " <>) . T.pack

        mkDesc :: DescriptionWithContext -> T.Text
        mkDesc (DescriptionWithContext d _) = "next step: " <> pretty d

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
          let color = case fg of
                        Blue  -> "blue"
                        Red   -> "red"
                        Green -> "green"
          in
            "<FONT COLOR=\"" <> color <> "\">" <> escapeChars str <> "</FONT>"
        toDotHtml (Style Nothing _ False False, str) = escapeChars str

        mkFocus :: DescriptionWithContext -> Maybe Ptr
        mkFocus (DescriptionWithContext _ ctxt) = listToMaybe (reverse ctxt)

