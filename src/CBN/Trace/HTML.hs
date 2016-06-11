{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module CBN.Trace.HTML (toJS) where

import Data.List (intersperse)
import Text.Blaze (ToMarkup(..))
import Text.Blaze.Html5 (Html, toHtml, (!))
import Text.Blaze.Html.Renderer.String
import qualified Data.Map                     as Map
import qualified Text.Blaze.Html5             as H
import qualified Text.PrettyPrint.ANSI.Leijen as Pretty
import qualified Text.Blaze.Html5.Attributes as A

import CBN.Heap
import CBN.Language
import CBN.Pretty
import CBN.Trace

toJS :: String -> Trace -> String
toJS name = \tr ->
       "function " ++ name ++ "(frame) {\n"
    ++ go 0 tr
    ++ "}\n"
  where
    go :: Int -> Trace -> String
    go n (Trace (hp, e) c) =
         "if(frame == " ++ show n ++ ") {\n"
      ++ set "heap" (renderHtml (toHtml hp))
      ++ set "term" (renderHtml (toHtml e))
      ++ case c of
           TraceWHNF _    -> set "status" "whnf"      ++ "}\n"
           TraceStuck err -> set "status" (mkErr err) ++ "}\n"
           TraceStopped   -> set "status" "stopped"   ++ "}\n"
           TraceStep tr'  -> set "status" ""          ++ "}\n" ++ go (n + 1) tr'

    mkErr :: String -> String
    mkErr = ("error: " ++)

    set :: String -> String -> String
    set suffix val = innerHTML suffix ++ " = " ++ show val ++ ";\n"

    innerHTML :: String -> String
    innerHTML suffix = "document.getElementById(\"" ++ name ++ "_" ++ suffix ++ "\").innerHTML"

{-------------------------------------------------------------------------------
  Translating to HTML
-------------------------------------------------------------------------------}

instance ToMarkup a => ToMarkup (Heap a) where
  toMarkup (Heap heap) =
    H.table $
      mapM_ go (Map.toList heap)
    where
      go :: (Ptr, a) -> Html
      go (ptr, term) =
        H.tr $ do
          H.td $ toHtml ptr
          H.td $ toHtml term

instance ToMarkup Ptr  where toMarkup = toHtml . pretty
instance ToMarkup Var  where toMarkup = toHtml . pretty
instance ToMarkup Prim where toMarkup = toHtml . pretty

instance ToMarkup Con where
  toMarkup c = H.span ! A.style "color: darkred" $ toHtml (pretty c)

instance ToMarkup Term where
  toMarkup (TVar x)       = toHtml x
  toMarkup (TPtr ptr)     = toHtml ptr
  toMarkup (TApp e1 e2)   = punctuate " " $ map toHtml [e1, e2]
  toMarkup (TCon c es)    = punctuate " " $ toHtml c : map toMarkup es
  toMarkup (TPrim p es)   = punctuate " " $ toHtml p : map toMarkup es
  toMarkup (TLam x e)     = do "\\" ; toHtml x ; " " ; toHtml e
  toMarkup (TLet x e1 e2) = do kw "let " ; toHtml x ; " = " ; toHtml e1 ;
                               kw "in" ; H.br
                               toHtml e2
  toMarkup (TCase e ms)   = do kw "case" ; toHtml e
                               kw "of" ; " {"
                               punctuate (H.br >> ";") (map toHtml ms)
                               " }"

instance ToMarkup Match where
  toMarkup (Match pat e) = do toMarkup pat ; " -> " ; toMarkup e

instance ToMarkup Pat where
  toMarkup (Pat c xs) = punctuate " " $ toHtml c : map toHtml xs

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | Keywords
kw :: String -> Html
kw s = H.b $ toHtml s

instance ToMarkup Pretty.Doc where
  toMarkup = toHtml . show

punctuate :: Html -> [Html] -> Html
punctuate sep = sequence_ . intersperse sep
