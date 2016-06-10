{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CBN.Language (
    -- * Variables
    Var(..)
    -- * Terms
  , Con
  , Pat(..)
  , Match(..)
  , Term(..)
    -- * Values
  , Value(..)
  , valueToTerm
  ) where

import Data.String (IsString)

import CBN.Heap

{-------------------------------------------------------------------------------
  Variables
-------------------------------------------------------------------------------}

-- | Variable
newtype Var = Var String
  deriving (Eq, Ord, IsString)

{-------------------------------------------------------------------------------
  Terms
-------------------------------------------------------------------------------}

-- | Constructor name
type Con = String

-- | Pattern
data Pat = Pat Con [Var]

-- | A single match in a case statement
data Match = Match Pat Term

-- | Term
data Term =
    TVar Var              -- ^ Variable
  | TApp Term Term        -- ^ Application
  | TLam Var Term         -- ^ Lambda abstraction
  | TPtr Ptr              -- ^ Heap pointer
  | TCon Con [Term]       -- ^ Constructor application
  | TPat Term [Match]     -- ^ Pattern match

{-------------------------------------------------------------------------------
  Values
-------------------------------------------------------------------------------}

-- | Values (terms in weak head normal form)
data Value =
    VLam Var Term
  | VCon Con [Term]

valueToTerm :: Value -> Term
valueToTerm (VLam x e)  = TLam x e
valueToTerm (VCon c es) = TCon c es
