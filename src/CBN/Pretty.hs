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

instance Pretty Ptr where
  pretty (Ptr (Just name) n) = text name <> text "@" <> pretty n
  pretty (Ptr Nothing     n) =              text "@" <> pretty n

instance Pretty Pat where
  pretty (Pat c xs) = hsep (pretty c : map pretty xs)

instance Pretty Prim where
  pretty (PInt n) = pretty n
  pretty PAdd     = text "add"

instance Pretty Term where
  pretty = go Top
    where
      go :: FixityContext -> Term -> Doc
      go _  (TVar x)       = pretty x
      go _  (TPtr n)       = pretty n
      go fc (TApp e1 e2)   = parensIf (needsParens fc Ap) $
                               go (L Ap) e1 <+> go (R Ap) e2
      go _  (TPrim p [])   = pretty p
      go fc (TPrim p ts)   = parensIf (needsParens fc Ap) $
                               hsep (pretty p : map (go (R Ap)) ts)
      go _  (TCon c [])    = pretty c
      go fc (TCon c es)    = parensIf (needsParens fc Ap) $
                               hsep (pretty c : map (go (R Ap)) es)
      go fc (TLam x e)     = parensIf (needsParens fc Lam) $
                               backslash <> pretty x <+> text "->" <+> go (R Lam) e
      go fc (TLet x e1 e2) = parensIf (needsParens fc Let) $
                                   text "let" <+> pretty x <+> text "=" <+> go (L Let) e1
                               </> text "in" <+> go (R Let) e2
      go fc (TPat e ms)    = parensIf (needsParens fc Case) $
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

{-------------------------------------------------------------------------------
  Dealing with precedence

  Adapted from
  <https://mail.haskell.org/pipermail/haskell-cafe/2008-January/038501.html>.
-------------------------------------------------------------------------------}

data Operator        = Ap | Lam | Let | Case              deriving Eq
data Assoc           = AssocLeft | AssocRight | AssocNone deriving Eq
data FixityContext   = Top | L Operator | R Operator
type PartialOrdering = Maybe Ordering

assoc :: Operator -> Assoc
assoc Ap   = AssocLeft
assoc Lam  = AssocRight
assoc Case = AssocRight
assoc Let  = AssocRight

comparePrec :: Operator -> Operator -> PartialOrdering
comparePrec op1 op2 | op1 == op2 = Just EQ
comparePrec Ap  _   = Just GT
comparePrec _   Ap  = Just LT
comparePrec _   _   = Just EQ

needsParens :: FixityContext -> Operator -> Bool
needsParens Top _  = False
needsParens (L ctxt) op
    | comparePrec ctxt op == Just LT = False
    | comparePrec ctxt op == Just GT = True
    | comparePrec ctxt op == Nothing = True
    -- otherwise the two operators have the same precedence
    | assoc ctxt /= assoc op         = True
    | assoc ctxt == AssocLeft        = False
    | otherwise                      = True
needsParens (R ctxt) op
    | comparePrec ctxt op == Just LT = False
    | comparePrec ctxt op == Just GT = True
    | comparePrec ctxt op == Nothing = True
    -- otherwise the two operators have the same precedence
    | assoc ctxt /= assoc op         = True
    | assoc ctxt == AssocRight       = False
    | otherwise                      = True
