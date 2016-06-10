{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CBN.Language (
    -- * Variables
    Var(..)
    -- * Terms
  , Con(..)
  , Pat(..)
  , Match(..)
  , Term(..)
  , nTApp
    -- * Values
  , Value(..)
  , valueToTerm
  ) where

import Data.Data (Data)
import Data.String (IsString)

import CBN.Heap

{-------------------------------------------------------------------------------
  Variables
-------------------------------------------------------------------------------}

-- | Variable
newtype Var = Var String
  deriving (Data, Eq, Ord, IsString)

{-------------------------------------------------------------------------------
  Terms
-------------------------------------------------------------------------------}

-- | Constructor name
newtype Con = Con String
    deriving (Data, Eq, Ord)

-- | Pattern
data Pat = Pat Con [Var]
    deriving (Data)

-- | A single match in a case statement
data Match = Match Pat Term
    deriving (Data)

-- | Term
data Term =
    TVar Var              -- ^ Variable
  | TApp Term Term        -- ^ Application
  | TLam Var Term         -- ^ Lambda abstraction
  | TPtr Ptr              -- ^ Heap pointer
  | TCon Con [Term]       -- ^ Constructor application
  | TPat Term [Match]     -- ^ Pattern match
  deriving (Data)

-- n-ary application
nTApp :: [Term] -> Term
nTApp []     = error "impossible"
nTApp [t]    = t
nTApp (t:ts) = t `TApp` nTApp ts

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
