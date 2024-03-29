{-# LANGUAGE CPP #-}
module CBN.Pretty (ToDoc, toDoc, heapToDoc) where

import Data.List (intersperse)
import Data.Set (Set)

import qualified Data.Map as Map
import qualified Data.Set as Set

import CBN.Closure
import CBN.Eval
import CBN.Heap
import CBN.Language
import CBN.Pretty.Precedence as P
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
  toDoc (Con "Nil")  = doc "[]"
  toDoc (Con "Unit") = doc "()"
  toDoc (Con c) = style (\st -> st { styleForeground = Just Red }) $ doc c

instance ToDoc Prim where
  toDoc (PInt n) = doc (show n)
  toDoc PISucc   = doc "succ"
  toDoc PIAdd    = doc "add"
  toDoc PISub    = doc "sub"
  toDoc PIMul    = doc "mul"
  toDoc PIMin    = doc "min"
  toDoc PIMax    = doc "max"
  toDoc PIEq     = doc "eq"
  toDoc PILt     = doc "lt"
  toDoc PILe     = doc "le"

instance ToDoc PrimApp where
  toDoc' fc (PrimApp PIAdd [a, b]) = parensIf (needsParens fc Add) $
    toDoc' (L Add) a <+> doc "+" <+> toDoc' (R Add) b
  toDoc' fc (PrimApp PISub [a, b]) = parensIf (needsParens fc Sub) $
    toDoc' (L Sub) a <+> doc "-" <+> toDoc' (R Sub) b
  toDoc' fc (PrimApp PIMul [a, b]) = parensIf (needsParens fc Mul) $
    toDoc' (L Mul) a <+> doc "*" <+> toDoc' (R Mul) b
  toDoc' fc (PrimApp PILe [a, b]) = parensIf (needsParens fc Le) $
    toDoc' (L Le) a <+> doc "<=" <+> toDoc' (R Le) b
  toDoc' fc (PrimApp PILt [a, b]) = parensIf (needsParens fc Lt) $
    toDoc' (L Lt) a <+> doc "<" <+> toDoc' (R Lt) b
  toDoc' fc (PrimApp PIEq [a, b]) = parensIf (needsParens fc Eq) $
    toDoc' (L Eq) a <+> doc "==" <+> toDoc' (R Eq) b
  toDoc' fc (PrimApp p es) = parensIf (needsParens fc P.Ap && not (null es)) $
    hsep (toDoc p : map (toDoc' (R P.Ap)) es)

instance ToDoc ConApp where
  toDoc' fc (ConApp (Con "Cons") [x, xs]) = parensIf (needsParens fc Cons) $
    toDoc' (L Cons) x <+> doc ":" <+> toDoc' (R Cons) xs
  toDoc' _fc (ConApp (Con "Pair") [x, xs]) = parensIf True $
    toDoc' Top x <> doc "," <+> toDoc' Top xs
  toDoc' fc (ConApp c es) = parensIf (needsParens fc P.Ap && not (null es)) $
    hsep (toDoc c : map (toDoc' (R P.Ap)) es)

instance ToDoc Pat where
  toDoc (Pat (Con "Cons") [x, xs]) =
    toDoc x <> doc ":" <> toDoc xs
  toDoc (Pat (Con "Pair") [x, xs]) = parensIf True $
    toDoc x <> doc "," <+> toDoc xs
  toDoc (Pat c xs) =
    hsep (toDoc c : map toDoc xs)

instance ToDoc Match where
  toDoc' fc = mconcat . matchRow fc

-- | Table-row for a match statement
--
-- Used when using a vertical layout for a case statement
matchRow :: FixityContext -> Match -> [Doc Style String]
matchRow fc (Match p rhs) = [toDoc p, doc " -> ", toDoc' fc rhs]

-- | Table-row for a multiple-binder let statement
letRow :: (Var, Term) -> [Doc Style String]
letRow (x, t) = [toDoc x, doc " = ", toDoc t]

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

  -- special case for @bind e1 (\x -> e2)@
  toDoc' fc (TApp (TApp (TPtr bind@(Ptr Nothing (Just "bind"))) e1) (TLam x e2)) =
    parensIfChoice (needsParens fc P.Ap) $ [
        stack [
            toDoc bind <+> toDoc' (R P.Ap) e1 <+> doc "(\\" <> toDoc x <+> doc "->"
          , toDoc' (R Lam) e2 <> doc ")"
          ]
      ]

  -- standard rendering
  toDoc' fc (TApp e1 e2) = parensIf (needsParens fc P.Ap) $
      toDoc' (L P.Ap) e1 <+> toDoc' (R P.Ap) e2
  toDoc' fc (TSeq e1 e2) = parensIf (needsParens fc P.Ap) $
      kw "seq" <+> toDoc' (R P.Ap) e1 <+> toDoc' (R P.Ap) e2
  toDoc' fc (TLam x e) = parensIf (needsParens fc Lam) $
      doc "\\" <> hsep (map toDoc (x:xs)) <+> doc "->" <+> toDoc' (R Lam) e'
    where
      (xs, e') = collectArgs e
  toDoc' fc (TLet [(x, e1)] e2) = parensIfChoice (needsParens fc Let) [
        stack [
            kw "let" <+> x' <+> doc "=" <+> e1' <+> kw "in"
          , e2'
          ]
      , kw "let" <+> x' <+> doc "=" <+> e1' <+> kw "in" <+> e2'
      ]
    where
      x'  = toDoc x
      e1' = toDoc' Top     e1
      e2' = toDoc' (R Let) e2
  toDoc' fc (TLet bound e) = parensIf (needsParens fc Let) $
      stack [
        kw "let" <+> doc "{"
      , indent $ table $ map letRow bound
      , doc "}" <+> kw "in" <+> toDoc' (R Let) e
      ]
  toDoc' fc (TCase e (Matches ms)) = parensIfChoice (needsParens fc Case) [
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
  toDoc' fc (TCase e (Selector s)) = parensIf (needsParens fc P.Ap) $
      toDoc' (L P.Ap) s <+> toDoc' (R P.Ap) e
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
      c' = toDoc' Top    c
      t' = toDoc' (R If) t
      f' = toDoc' (R If) f

instance ToDoc Selector where
  toDoc Fst = doc "fst"
  toDoc Snd = doc "snd"

instance ToDoc Closure where
  toDoc cl = case cl of
    ErrorClosure str -> doc "Error :" <+> doc str
    FunClosure term _ -> doc "Function :" <+> toDoc term
    ConClosure con _ -> doc "Constructor :" <+> toDoc con
    IndirectionClosure _ -> doc "Indirection " -- <+> toDoc ptr
    ThunkClosure term _ -> doc "Thunk :" <+> toDoc term
    PrimClosure prim _ -> doc "Primary :" <+> toDoc prim

instance ToDoc Description where
  toDoc StepAlloc        = doc "allocate"
  toDoc StepBeta         = doc "beta reduction"
  toDoc (StepApply f)    = doc "apply"  <+> toDoc f
  toDoc (StepDelta pes)  = doc "delta:" <+> toDoc pes
  toDoc (StepMatch c)    = doc "match"  <+> toDoc c
  toDoc (StepIf b)       = doc "if"     <+> doc (show b)
  toDoc StepSeq          = doc "seq"
  toDoc StepAllocConArgs = doc "allocate constructor arguments"

-- | Based on purescript implementation
mintersperse :: (Monoid m) => m -> [m] -> m
mintersperse _ []       = mempty
mintersperse _ [x]      = x
mintersperse sep (x:xs) = x <> sep <> mintersperse sep xs

instance ToDoc DescriptionWithContext where
  toDoc (DescriptionWithContext descr []) = toDoc descr
  toDoc (DescriptionWithContext descr context) = mconcat [
        toDoc descr
      , doc " in ["
      , mintersperse (doc ", ") $ map toDoc context
      , doc "]"
      ]

-- | For the heap we need to know which pointers we are about to collect
heapToDoc :: forall a. ToDoc a
  => Set Ptr   -- ^ To be collected
  -> Maybe Ptr -- ^ Focus (where are we going to take a step?)
  -> Heap a -> Doc Style String
heapToDoc garbage focus (Heap _next heap) =
    table $ map go (Map.toList heap)
  where
    go :: (Ptr, a) -> [Doc Style String]
    go (ptr, a) = [mark ptr $ toDoc ptr, doc " = ", toDoc a]

    mark :: Ptr -> Doc Style String -> Doc Style String
    mark ptr
      | ptr `Set.member` garbage = style $ \st -> st { styleBackground = Just Red }
      | Just ptr == focus        = style $ \st -> st { styleBackground = Just Green }
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
