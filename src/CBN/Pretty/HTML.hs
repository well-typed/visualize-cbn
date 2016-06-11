{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module CBN.Pretty.HTML (toHtml) where

import Data.List (intersperse)
import Text.Blaze (ToMarkup(..))
import Text.Blaze.Html5 (Html, toHtml, (!))
import qualified Data.Map                     as Map
import qualified Text.Blaze.Html5             as H
import qualified Text.PrettyPrint.ANSI.Leijen as Pretty
import qualified Text.Blaze.Html5.Attributes as A

import CBN.Heap
import CBN.Language
import CBN.Pretty.Doc

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

instance ToMarkup Prim where toMarkup = toHtml . pretty

instance ToMarkup Var where
  toMarkup x = H.i $ toHtml (pretty x)

instance ToMarkup Ptr where
  toMarkup ptr = H.span ! A.style "color: darkblue" $ go ptr
    where
      go :: Ptr -> Html
      go (Ptr Nothing  Nothing)     = error "invalid pointer"
      go (Ptr (Just n) Nothing)     = toHtml n
      go (Ptr Nothing  (Just name)) = toHtml name
      go (Ptr (Just n) (Just name)) = do toHtml name ; "_" ; toHtml n

instance ToMarkup Con where
  toMarkup c = H.span ! A.style "color: darkred" $ toHtml (pretty c)

instance ToMarkup Term where
  toMarkup (TVar x)       = toHtml x
  toMarkup (TPtr ptr)     = toHtml ptr
  toMarkup (TApp e1 e2)   = punctuate " " $ map toHtml [e1, e2]
  toMarkup (TCon c es)    = punctuate " " $ toHtml c : map toMarkup es
  toMarkup (TPrim p es)   = punctuate " " $ toHtml p : map toMarkup es
  toMarkup (TLam x e)     = do "\\" ; toHtml x ; " -> " ; toHtml e
  toMarkup (TLet x e1 e2) = do kw "let " ; toHtml x ; " = " ; toHtml e1 ;
                               kw "in" ; H.br
                               toHtml e2
  toMarkup (TCase e ms)   = do kw "case " ; toHtml e
                               kw " of " ; " {"
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
