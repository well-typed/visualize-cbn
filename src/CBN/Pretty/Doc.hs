module CBN.Pretty.Doc (pretty) where

import Text.PrettyPrint.ANSI.Leijen
import qualified Data.Map as Map

import CBN.Language
import CBN.Heap
import CBN.Pretty.Precedence

instance Pretty Var where pretty (Var x) = text x
instance Pretty Con where pretty (Con c) = text c

instance Pretty Ptr where
  pretty (Ptr Nothing  Nothing)     = error "invalid pointer"
  pretty (Ptr (Just n) Nothing)     = pretty n
  pretty (Ptr Nothing  (Just name)) =             text "@" <> text name
  pretty (Ptr (Just n) (Just name)) = pretty n <> text "@" <> text name

instance Pretty Pat where
  pretty (Pat c xs) = hsep (pretty c : map pretty xs)

instance Pretty Prim where
  pretty (PInt n) = pretty n
  pretty PIAdd    = text "add"
  pretty PIEq     = text "eq"
  pretty PILt     = text "lt"
  pretty PILe     = text "le"

instance Pretty Term where
  pretty = go Top
    where
      go :: FixityContext -> Term -> Doc
      go _  (TVar x)       = pretty x
      go _  (TPtr n)       = pretty n
      go fc (TApp e1 e2)   = parensIf (needsParens fc Ap) $
                               go (L Ap) e1 <+> go (R Ap) e2
      go fc (TPrim p es)   = parensIf (needsParens fc Ap && not (null es)) $
                               hsep (pretty p : map (go (R Ap)) es)
      go fc (TCon c es)    = parensIf (needsParens fc Ap && not (null es)) $
                               hsep (pretty c : map (go (R Ap)) es)
      go fc (TLam x e)     = parensIf (needsParens fc Lam) $
                               backslash <> pretty x <+> text "->" <+> go (R Lam) e
      go fc (TLet x e1 e2) = parensIf (needsParens fc Let) $
                                   text "let" <+> pretty x <+> text "=" <+> go (L Let) e1
                               </> text "in" <+> go (R Let) e2
      go fc (TCase e ms)   = parensIf (needsParens fc Case) $
                                   (text "case" <+> go (L Case) e <+> lbrace)
                               </> align (goMatches ms)
                               </> rbrace

      goMatch :: Match -> Doc
      goMatch (Match pat term) = pretty pat <+> text "->" <+> go (R Case) term

      goMatches :: [Match] -> Doc
      goMatches = mconcat
                . punctuate (semi <> softline)
                . map (indent 2 . goMatch)

instance Pretty a => Pretty (Heap a) where
  pretty (Heap heap) = vcat $ map go (Map.toList heap)
    where
      go :: (Ptr, a) -> Doc
      go (ptr, a) = pretty ptr <> indent 8 (pretty a)

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

parensIf :: Bool -> Doc -> Doc
parensIf False = id
parensIf True  = parens
