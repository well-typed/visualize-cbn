module CBN.Pretty (
    -- * Re-exportsr
    pretty
  ) where

import Text.PrettyPrint.ANSI.Leijen
import qualified Data.Map as Map

import CBN.Language
import CBN.Heap

instance Pretty Var where pretty (Var x) = text x
instance Pretty Con where pretty (Con c) = text c
instance Pretty Ptr where pretty (Ptr n) = text "@" <> pretty n

instance Pretty Pat where
  pretty (Pat c xs) = hsep (pretty c : map pretty xs)

instance Pretty Match where
  pretty (Match pat term) = pretty pat <+> text "->" <+> pretty term

instance Pretty Prim where
  pretty (PInt n) = pretty n
  pretty PAdd     = text "add"

instance Pretty Term where
  pretty = go pTop
    where
      go :: Int -> Term -> Doc
      go _ (TVar x)        = pretty x
      go _ (TPtr n)        = pretty n
      go p (TPrim prim ts) = parensIf (p > pPrim) $
                               hsep (pretty prim : map (go pPrim) ts)
      go p (TApp e1 e2)    = parensIf (p > pApp) $
                               go pApp e1 <+> go pApp e2
      go p (TLam x e)      = parensIf (p > pLam) $
                               backslash <> pretty x <+> text "->" <+> go pLam e
      go p (TLet x e1 e2)  = parensIf (p > pLet) $
                                   text "let" <+> pretty x <+> text "=" <+> pretty e1
                               </> text "in" <+> pretty e2
      go p (TCon c es)     = parensIf (p > pCon) $
                               hsep (pretty c : map (go pCon) es)
      go p (TPat e ms)     = parensIf (p > pPat) $
                                   (text "case" <+> go pTop e <+> lbrace)
                               </> align (goMatches ms)
                               </> rbrace

      goMatches :: [Match] -> Doc
      goMatches []     = empty
      goMatches [m]    = pretty m
      goMatches (m:ms) = indent 2 (pretty m)
                     </> text ";" <+> goMatches ms

      -- operator precedence
      pApp  = 6
      pCon  = 5
      pPrim = 4
      pPat  = 3
      pLet  = 2
      pLam  = 1
      pTop  = 0

instance Pretty a => Pretty (Heap a) where
  pretty (Heap heap) = vcat $ map go (Map.toList heap)
    where
      go :: (Ptr, a) -> Doc
      go (ptr, a) = pretty ptr <> indent 8 (pretty a)

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

parensIf :: Bool -> Doc -> Doc
parensIf b = parens
--parensIf False = id
--parensIf True  = parens
