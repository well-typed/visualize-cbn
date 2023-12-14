{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CBN.Language (
    -- * Variables
    Var(..)
    -- * Terms
  , Con(..)
  , Pat(..)
  , Match(..)
  , Prim(..)
  , ConApp(..)
  , PrimApp(..)
  , Term(..)
  , Branches(..)
  , Selector(..)
    -- * Classification
  , termIsSimple
    -- * Interpretation of selectors
  , selectorCon
  , selectorVars
  , selectorIndex
  , selectorMatch
    -- * Values
  , Value(..)
  , valueToTerm
    -- * Auxiliary
  , nTApp
  , collectArgs
    -- * Lifting from the meta language to the object language
  , liftInt
  , liftBool
  ) where

import Data.Bifunctor
import Data.Data (Data(..))
import Data.String (IsString)

import CBN.Heap
import CBN.Util.Snoc (Snoc)

import qualified CBN.Util.Snoc as Snoc

{-------------------------------------------------------------------------------
  Variables
-------------------------------------------------------------------------------}

-- | Variable
newtype Var = Var { varName :: String }
  deriving (Show, Data, Eq, Ord, IsString)

{-------------------------------------------------------------------------------
  Terms
-------------------------------------------------------------------------------}

-- | Constructor name
newtype Con = Con { conName :: String }
  deriving (Show, Data, Eq, Ord)

-- | Pattern
data Pat = Pat Con [Var]
  deriving (Show, Data, Eq)

-- | A single match in a case statement
data Match = Match Pat Term
  deriving (Show, Data, Eq)

-- | Primitives
data Prim =
    PInt Integer
  | PISucc
  | PIAdd
  | PISub
  | PIMul
  | PIMin
  | PIMax
  | PIEq
  | PILt
  | PILe
  deriving (Show, Data, Eq)

-- | Application of a constructor to some arguments
data ConApp = ConApp Con [Term]
  deriving (Show, Data, Eq)

-- | Application of a primitive to some arguments
data PrimApp = PrimApp Prim [Term]
  deriving (Show, Data, Eq)

-- | Term
data Term =
    TVar Var                 -- ^ Variable
  | TApp Term Term           -- ^ Application
  | TLam Var Term            -- ^ Lambda abstraction
  | TLet [(Var, Term)] Term  -- ^ (Mutually recursive) let binding
  | TPtr Ptr                 -- ^ Heap pointer
  | TCon ConApp              -- ^ Constructor application
  | TCase Term Branches      -- ^ Pattern match
  | TPrim PrimApp            -- ^ Primitives (built-ins)
  | TIf Term Term Term       -- ^ Conditional
  | TSeq Term Term           -- ^ Force evaluation
  deriving (Show, Data, Eq)

-- | Branches of a case statement
data Branches =
    -- | User-defined branches (normal case statement)
    Matches [Match]

    -- | Selector
  | Selector Selector
  deriving (Show, Data, Eq)

-- | Selectors
data Selector =
    Fst
  | Snd
  deriving (Show, Data, Eq)

{-------------------------------------------------------------------------------
  Classification
-------------------------------------------------------------------------------}

-- | Is this a "simple" term?
--
-- A simple term is one that we can substitute freely, even if multiple times,
-- without losing sharing.
termIsSimple :: Term -> Bool
termIsSimple (TPtr _)               = True
termIsSimple (TCon (ConApp _ []))   = True
termIsSimple (TPrim (PrimApp _ [])) = True
termIsSimple _                      = False

{-------------------------------------------------------------------------------
  Interpretation of selectors
-------------------------------------------------------------------------------}

-- | Constructor name this selector matches against
selectorCon :: Selector -> Con
selectorCon Fst = Con "Pair"
selectorCon Snd = Con "Pair"

-- | Variable names for the implied case statement of this selector
selectorVars :: Selector -> [Var]
selectorVars Fst = [Var "x", Var "y"]
selectorVars Snd = [Var "x", Var "y"]

-- | Which argument does this selector extract?
selectorIndex :: Selector -> Int
selectorIndex Fst = 0
selectorIndex Snd = 1

selectorMatch :: Selector -> Match
selectorMatch s =
    Match
      (Pat (selectorCon s) (selectorVars s))
      (TVar $ selectorVars s !! selectorIndex s)

{-------------------------------------------------------------------------------
  Values
-------------------------------------------------------------------------------}

-- | Values (terms in weak head normal form)
data Value =
    -- | Lambda abstractions are values
    VLam Var Term

    -- | Constructor applications are values
  | VCon ConApp

    -- | Primitive values are values
    --
    -- Note that an application of a primitive value to some term is NOT a
    -- value: primitive functions are assumed strict in all arguments
  | VPrim Prim
  deriving (Show, Eq)

valueToTerm :: Value -> Term
valueToTerm (VLam x e) = TLam x e
valueToTerm (VCon ces) = TCon ces
valueToTerm (VPrim p)  = TPrim (PrimApp p [])

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | n-ary application
nTApp :: [Term] -> Term
nTApp = go . Snoc.fromList
  where
    go :: Snoc Term -> Term
    go Snoc.Nil               = error "impossible"
    go (Snoc.Cons Snoc.Nil t) = t
    go (Snoc.Cons ts       t) = go ts `TApp` t

-- | Collect all arguments for a lambda application
-- (as if we had n-ary lambdas)
collectArgs :: Term -> ([Var], Term)
collectArgs (TLam x e) = first (x:) $ collectArgs e
collectArgs e          = ([], e)

{-------------------------------------------------------------------------------
  Lifting from Haskell to our object language
-------------------------------------------------------------------------------}

liftInt :: Integer -> Value
liftInt = VPrim . PInt

liftBool :: Bool -> Value
liftBool True  = VCon $ ConApp (Con "True")  []
liftBool False = VCon $ ConApp (Con "False") []
