module CBN.Heap (
    -- * Heap
    Ptr(..)
  , Heap(..)
  , emptyHeap
  , deref
  , alloc
  , mutate
  ) where

import Data.Map (Map)
import qualified Data.Map as Map

{-------------------------------------------------------------------------------
  Heap
-------------------------------------------------------------------------------}

-- | Heap pointer
newtype Ptr = Ptr Int
  deriving (Eq, Ord)

-- | Heap
--
-- NOTE: We will use the convention that if a particular term or pointer is
-- to be interpreted in a specific heap, we will tuple the two.
newtype Heap a = Heap (Map Ptr a)

emptyHeap :: Heap a
emptyHeap = Heap Map.empty

alloc :: Heap a -> a -> (Heap a, Ptr)
alloc (Heap hp) e = (Heap (Map.insert ptr e hp), ptr)
  where
    ptr :: Ptr
    ptr = Ptr (Map.size hp)

deref :: (Heap a, Ptr) -> a
deref (Heap hp, ptr) = hp Map.! ptr

mutate :: (Heap a, Ptr) -> a -> Heap a
mutate (Heap hp, ptr) term = Heap (Map.insert ptr term hp)
