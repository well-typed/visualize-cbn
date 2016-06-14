-- | Dealing with precedence
--
-- Adapted from
-- <https://mail.haskell.org/pipermail/haskell-cafe/2008-January/038501.html>.
module CBN.Pretty.Precedence (
    FixityContext(..)
  , Operator(..)
  , needsParens
  ) where

type PartialOrdering = Maybe Ordering

-- | The operators we use in our language
data Operator =
    Ap
  | Lam
  | Let
  | Case
  | If
  | Cons
  | Add
  | Sub
  | Mul
  | Le
  | Lt
  | Eq
  deriving Eq

-- | Associativty
data Assoc =
    AssocLeft
  | AssocRight
  | AssocNone
  deriving Eq

-- | The context in which we are pretty-printing a term
data FixityContext = Top | L Operator | R Operator

assoc :: Operator -> Assoc
assoc Ap   = AssocLeft
assoc Lam  = AssocRight
assoc Case = AssocRight
assoc Let  = AssocRight
assoc If   = AssocRight
assoc Cons = AssocRight
assoc Add  = AssocRight
assoc Mul  = AssocRight
assoc Sub  = AssocNone
assoc Le   = AssocNone
assoc Lt   = AssocNone
assoc Eq   = AssocNone

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
