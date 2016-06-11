module CBN.Heap (
    -- * Heap
    Ptr(..)
  , Heap(..)
  , emptyHeap
  , deref
  , alloc
  , mutate
  ) where

import Data.Data (Data)
import Data.Map (Map)
import qualified Data.Map as Map

{-------------------------------------------------------------------------------
  Heap
-------------------------------------------------------------------------------}

-- | Heap pointer
--
-- To improve readability, we keep an optional name for pointers that correspond
-- to variables in the user's code.
data Ptr = Ptr (Maybe String) Int
  deriving (Show, Eq, Ord, Data)

-- | Heap
--
-- NOTE: We will use the convention that if a particular term or pointer is
-- to be interpreted in a specific heap, we will tuple the two.
newtype Heap a = Heap (Map Ptr a)
  deriving (Show)

emptyHeap :: Heap a
emptyHeap = Heap Map.empty

-- | Allocate a new value on the heap
--
-- The value is allowed to depend on the new heap pointer.
alloc :: Maybe String -> Heap a -> (Ptr -> a) -> (Heap a, Ptr)
alloc name (Heap hp) e = (Heap (Map.insert ptr (e ptr) hp), ptr)
  where
    ptr :: Ptr
    ptr = Ptr name (Map.size hp)

deref :: (Heap a, Ptr) -> a
deref (Heap hp, ptr) = hp Map.! ptr

mutate :: (Heap a, Ptr) -> a -> Heap a
mutate (Heap hp, ptr) term = Heap (Map.insert ptr term hp)
