module CBN.Pretty (ToDoc, toDoc, heapToDoc) where

import Data.Monoid
import Data.List (intersperse)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set

import CBN.Eval
import CBN.Heap
import CBN.Language
import CBN.Pretty.Precedence
import CBN.Util.Doc
import CBN.Util.Doc.Style

class ToDoc a where
  toDoc :: a -> Doc Style String
  toDoc = toDoc' Top

  toDoc' :: FixityContext -> a -> Doc Style String
  toDoc' _fc = toDoc

-- | For convenience, 'ToDoc' is idempotent
instance ToDoc (Doc Style String) where
  toDoc = id

instance ToDoc Var where
  toDoc (Var x) = style (\st -> st { styleItalic = True }) $ doc x

instance ToDoc Con where
  toDoc (Con c) = style (\st -> st { styleForeground = Just Red }) $ doc c

instance ToDoc Prim where
  toDoc (PInt n) = doc (show n)
  toDoc PIAdd    = doc "add"
  toDoc PIEq     = doc "eq"
  toDoc PILt     = doc "lt"
  toDoc PILe     = doc "le"

instance ToDoc PrimApp where
  toDoc' fc (PrimApp p es) = parensIf (needsParens fc Ap && not (null es)) $
    hsep (toDoc p : map (toDoc' (R Ap)) es)

instance ToDoc ConApp where
  toDoc' fc (ConApp c es) = parensIf (needsParens fc Ap && not (null es)) $
    hsep (toDoc c : map (toDoc' (R Ap)) es)

instance ToDoc Pat where
  toDoc (Pat c xs) = hsep (toDoc c : map toDoc xs)

instance ToDoc Match where
  toDoc' fc = mconcat . matchRow fc

-- | Table-row for a match statement
--
-- Used when using a vertical layout for a case statement
matchRow :: FixityContext -> Match -> [Doc Style String]
matchRow fc (Match p rhs) = [toDoc p, doc " -> ", toDoc' fc rhs]

-- | We make elements from the prelude blue
instance ToDoc Ptr where
  toDoc (Ptr Nothing  Nothing)     = error "invalid pointer"
  toDoc (Ptr (Just n) Nothing)     = doc (show n)
  toDoc (Ptr Nothing  (Just name)) = style (\st -> st { styleForeground = Just Blue })
                                   $ doc name
  toDoc (Ptr (Just n) (Just name)) = doc name <> doc "_" <> doc (show n)

instance ToDoc Term where
  toDoc' _  (TVar x)       = toDoc x
  toDoc' _  (TPtr n)       = toDoc n
  toDoc' fc (TPrim pes )   = toDoc' fc pes
  toDoc' fc (TCon ces)     = toDoc' fc ces
  toDoc' fc (TApp e1 e2) = parensIf (needsParens fc Ap) $
      toDoc' (L Ap) e1 <+> toDoc' (R Ap) e2
  toDoc' fc (TSeq e1 e2) = parensIf (needsParens fc Ap) $
      kw "seq" <+> toDoc' (R Ap) e1 <+> toDoc' (R Ap) e2
  toDoc' fc (TLam x e) = parensIf (needsParens fc Lam) $
      doc "\\" <> hsep (map toDoc (x:xs)) <+> doc "->" <+> toDoc' (R Lam) e'
    where
      (xs, e') = collectArgs e
  toDoc' fc (TLet x e1 e2) = parensIfChoice (needsParens fc Let) [
        stack [
            kw "let" <+> x' <+> doc "=" <+> e1' <+> kw "in"
          , e2'
          ]
      , kw "let" <+> x' <+> doc "=" <+> e1' <+> kw "in" <+> e2'
      ]
    where
      x'  = toDoc x
      e1' = toDoc' (L Let) e1
      e2' = toDoc' (R Let) e2
  toDoc' fc (TCase e ms) = parensIfChoice (needsParens fc Case) [
        stack [
            kw "case" <+> e' <+> kw "of" <+> doc "{"
          , indent $ table $ map (matchRow (R Case)) ms
          , doc "}"
          ]
      , kw "case" <+> e' <+> kw "of" <+> wrap "{ " " }" (punctuate " ; " ms')
      ]
    where
      e'  = toDoc' (L Case) e
      ms' = map (toDoc' (R Case)) ms
  toDoc' fc (TIf c t f) = parensIfChoice (needsParens fc If) [
        stack [
            kw "if" <+> c'
          , indent $ stack [
                kw "then" <+> t'
              , kw "else" <+> f'
              ]
          ]
      , kw "if" <+> c' <+> kw "then" <+> t' <+> kw "else" <+> f'
      ]
    where
      c' = toDoc' (L If) c
      t' = toDoc' (R If) t
      f' = toDoc' (R If) f

instance ToDoc Description where
  toDoc StepAlloc        = doc "allocate"
  toDoc StepBeta         = doc "beta reduction"
  toDoc (StepApply f)    = doc "apply"  <+> toDoc f
  toDoc (StepDelta p ps) = doc "delta:" <+> hsep (map toDoc (p:ps))
  toDoc (StepMatch c)    = doc "match"  <+> toDoc c
  toDoc (StepIf b)       = doc "if"     <+> doc (show b)
  toDoc StepSeq          = doc "seq"

-- | For the heap we need to know which pointers we are about to collect
heapToDoc :: forall a. ToDoc a => Set Ptr -> Heap a -> Doc Style String
heapToDoc garbage (Heap _next heap) =
    table $ map go (Map.toList heap)
  where
    go :: (Ptr, a) -> [Doc Style String]
    go (ptr, a) = [markGarbage ptr $ toDoc ptr, doc " = ", toDoc a]

    markGarbage :: Ptr -> Doc Style String -> Doc Style String
    markGarbage ptr
      | ptr `Set.member` garbage = style $ \st -> st { styleBackground = Just Red }
      | otherwise                = id

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

kw :: String -> Doc Style String
kw = style (\st -> st { styleBold = True }) . doc

parensIf :: Bool -> Doc Style String -> Doc Style String
parensIf False = id
parensIf True  = wrap "(" ")"

-- | Swap the order of the choices if we need parentheses
--
-- The idea is that we prefer a multi-line layout normally, but if we
-- need to insert parentheses we prefer a single-line layout.
parensIfChoice :: Bool -> [Doc Style String] -> Doc Style String
parensIfChoice p ds = parensIf p $ choice $ (if p then reverse else id) ds

wrap :: String -> String -> Doc Style String -> Doc Style String
wrap lft rgt d = doc lft <> d <> doc rgt

punctuate :: String -> [Doc Style String] -> Doc Style String
punctuate sep = mconcat . intersperse (doc sep)

hsep :: [Doc Style String] -> Doc Style String
hsep = punctuate " "

indent :: Doc Style String -> Doc Style String
indent = (doc "  " <>)

(<+>) :: Doc Style String -> Doc Style String -> Doc Style String
(<+>) d1 d2 = d1 <> doc " " <> d2
