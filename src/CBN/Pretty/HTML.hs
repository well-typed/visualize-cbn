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
import CBN.Eval
import CBN.Language
import CBN.Pretty.Doc
import CBN.Pretty.Precedence

{-------------------------------------------------------------------------------
  Translating to HTML
-------------------------------------------------------------------------------}

instance ToMarkup a => ToMarkup (Heap a) where
  toMarkup (Heap _next heap) =
    H.table $
      mapM_ go (Map.toList heap)
    where
      go :: (Ptr, a) -> Html
      go (ptr, term) =
        H.tr $ do
          H.td $ toHtml ptr
          H.td $ "="
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
  toMarkup = go Top
    where
      go :: FixityContext -> Term -> Html
      go _  (TVar x)       = toHtml x
      go _  (TPtr ptr)     = toHtml ptr
      go fc (TApp e1 e2)   = parensIf (needsParens fc Ap) $
                               do go (L Ap) e1 ; " " ; go (R Ap) e2
      go fc (TPrim p es)   = parensIf (needsParens fc Ap && not (null es)) $
                               punctuate " " $ toHtml p : map (go (R Ap)) es
      go fc (TCon c es)    = parensIf (needsParens fc Ap && not (null es)) $
                               punctuate " " $ toHtml c : map (go (R Ap)) es
      go fc (TLam x e)     = parensIf (needsParens fc Lam) $ do
                               let (xs, e') = collectArgs e
                               "\\"
                               punctuate " " $ map toHtml (x:xs)
                               " -> "
                               go (R Lam) e'
      go fc (TLet x e1 e2) = parensIf (needsParens fc Let) $ do
                               kw "let " ; toHtml x ; " = " ; go (L Let) e1 ;
                               kw "in" ; H.br
                               go (R Let) e2
      go fc (TCase e ms)   = parensIf (needsParens fc Case) $ do
                                kw "case " ; go (L Case) e
                                kw " of " ; " {"
                                H.div $ punctuate (";" >> H.br) (map goMatch ms)
                                " }"
      go fc (TIf c t f)    = parensIf (needsParens fc If) $ do
                               kw "if " ; go (L If) c
                               kw " then " ; go (R Case) t
                               kw " else " ; go (R Case) f

      goMatch :: Match -> Html
      goMatch (Match pat e) = do nbsp ; nbsp ; toMarkup pat ; " -> " ; go (R Case) e

instance ToMarkup Pat where
  toMarkup (Pat c xs) = punctuate " " $ toHtml c : map toHtml xs

instance ToMarkup Description where
  toMarkup StepAlloc        = "allocate"
  toMarkup StepBeta         = "beta reduction"
  toMarkup (StepApply f)    = "apply " >> toHtml f
  toMarkup (StepDelta p ps) = "delta: " >> punctuate " " (map toHtml (p:ps))
  toMarkup StepMatch        = "match"
  toMarkup (StepIf b)       = "if " >> toHtml b

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

parensIf :: Bool -> Html -> Html
parensIf False html = html
parensIf True  html = do "(" ; html ; ")"

-- | Keywords
kw :: String -> Html
kw s = H.b $ toHtml s

instance ToMarkup Pretty.Doc where
  toMarkup = toHtml . show

punctuate :: Html -> [Html] -> Html
punctuate sep = sequence_ . intersperse sep

nbsp :: Html
nbsp = preEscapedToMarkup ("&nbsp;" :: String)
