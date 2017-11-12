{-# LANGUAGE OverloadedStrings #-}

module CBN.Trace.Graph (render) where

import Data.Set (Set)
import qualified Data.Set as Set
import CBN.Heap
import           CBN.Trace
import           CBN.Pretty
import CBN.Eval
import qualified Data.Sequence as Seq
import qualified Data.Text     as T
import qualified CBN.Util.Doc               as Doc
import           CBN.Util.Doc.Style
import qualified CBN.Util.Doc.Rendered      as Rendered
import Data.Monoid ((<>))
import Debug.Trace (trace)

data Node = Node

render :: Trace -> String
render tr@(Trace (hp, t) cont) =
  "digraph G {\n"
  -- ++ "rankdir=LR;\n"
  ++ "node [ shape=\"record\" ];\n"
  ++ go 0 tr
  ++ "}"
  where
    go :: Int -> Trace -> String
    go n (Trace (hp, t) cont) =
      case cont of
        TraceWHNF _     -> mkFrame Set.empty "whnf"
        TraceStuck err  -> mkFrame Set.empty (mkErr err)
        TraceStopped    -> mkFrame Set.empty "stopped"
        TraceStep d tr' -> mkFrame Set.empty (mkDesc d) ++ go (n + 1) tr'
        TraceGC ps tr'  -> mkFrame ps "gc" ++ go (n + 1) tr'
      where
        mkFrame :: Set Ptr -> T.Text -> String
        mkFrame garbage status =
          T.unpack $
            setLabel n ("{ " <> escapeChars (pretty t) <> " | " <> escapeChars (pretty (heapToDoc garbage hp)) <> " | " <> status <> " }")
            <> "\n"
            <> mkConnection n

        escapeChars :: T.Text -> T.Text
        escapeChars =
          T.replace ">" "\\>"
          . T.replace "{" "\\{"
          . T.replace "}" "\\}"
          . T.replace " " "\\ "
          . T.replace "\n" "\\l"
          . T.replace "\\" "\\\\"
          . (\a -> trace (T.unpack a) a)

        setLabel :: Int -> T.Text -> T.Text
        setLabel n label = mkNode n <> "[label=\"" <> label <> "\"];"

        mkConnection :: Int -> T.Text
        mkConnection n
          | n == 0 = mkNode 0 <> ";\n"
          | otherwise = mkNode (n - 1) <> " -> " <> mkNode n <> ";\n"

        mkNode :: Int -> T.Text
        mkNode n = "s" <> T.pack (show n)

        mkErr :: String -> T.Text
        mkErr = ("error: " <>) . T.pack

        mkDesc :: Description -> T.Text
        mkDesc d = "next step: " <> pretty d

        pretty :: ToDoc a => a -> T.Text
        pretty = T.pack . goRendered . Rendered.rendered . Doc.render (\r -> Rendered.width r <= 80) . toDoc

        goRendered :: [[Maybe (Style, Char)]] -> String
        goRendered [] = ""
        goRendered (row:xs) = goRow row ++ "\n" ++ goRendered xs

        goRow :: [Maybe (Style, Char)] -> String
        goRow [] = ""
        goRow (x:xs) = goChar x ++ goRow xs

        goChar :: Maybe (Style, Char) -> String
        goChar Nothing = " "
        goChar (Just (_st, c)) = [c]
  -- s0 -> s1;
  -- s1 [ label="{ case map (\\x -\> x + x) (repeat (10 + 1)) of \{\l\ \ x:xs' -\> x\l\}\l | (apply map) }"
  --    ];
