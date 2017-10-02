-- | Heap
--
-- Intended for unqualified import
module CBN.Heap (
    -- * Heap
    Ptr(..)
  , Heap(..)
  , emptyHeap
  , deref
  , alloc
  , mutate
  , initHeap
    -- * Garbage collection
  , Pointers(..)
  , markAndSweep
  ) where

import Data.Data (Data)
import Data.Map (Map)
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Graph (Graph)
import qualified Data.Foldable as Foldable
import qualified Data.Map      as Map
import qualified Data.Set      as Set
import qualified Data.Graph    as Graph

{-------------------------------------------------------------------------------
  Heap
-------------------------------------------------------------------------------}

-- | Heap pointer
--
-- To improve readability, we keep an optional name for pointers that correspond
-- to variables in the user's code.
--
-- The @Int@ part is intentionally first so that pointers introduced earlier
-- will be sorted first, independent of their name. This keeps the display of
-- the heap in chronological order.
--
-- The @Int@ part becauses we don't use it for elements added to the initial
-- heap.
data Ptr = Ptr (Maybe Int) (Maybe String)
  deriving (Show, Eq, Ord, Data)

-- | Heap
--
-- NOTE: We will use the convention that if a particular term or pointer is
-- to be interpreted in a specific heap, we will tuple the two.
data Heap a = Heap {
    -- | Next available pointer
    --
    -- We separately store the next available heap pointer, because when we
    -- do garbage collection working out which pointer is available is
    -- non-trivial. It's less confusing as well when pointers are never reused.
    heapNextAvailable :: Int

    -- | The actual entries on the heap
  , heapEntries :: Map Ptr a
  }
  deriving (Show)

emptyHeap :: Heap a
emptyHeap = Heap 0 Map.empty

-- | Allocate a new value on the heap
--
-- The value is allowed to depend on the new heap pointer.
alloc :: Maybe String -> Heap a -> (Ptr -> a) -> (Heap a, Ptr)
alloc name (Heap next hp) e =
    (Heap (next + 1) (Map.insert ptr (e ptr) hp), ptr)
  where
    ptr :: Ptr
    ptr = Ptr (Just next) name

deref :: (Heap a, Ptr) -> a
deref (Heap _ hp, ptr) =
    Map.findWithDefault (error $ "deref: invalid pointer " ++ show ptr) ptr hp

mutate :: (Heap a, Ptr) -> a -> Heap a
mutate (Heap next hp, ptr) term = Heap next (Map.insert ptr term hp)

initHeap :: [(String, a)] -> Heap a
initHeap = Heap 0 . Map.fromList . map aux
  where
    aux :: (String, a) -> (Ptr, a)
    aux (name, a) = (Ptr Nothing (Just name), a)

{-------------------------------------------------------------------------------
  Garbage collection
-------------------------------------------------------------------------------}

class Pointers a where
  pointers :: a -> Set Ptr

-- | Find all reachable pointers given a set of roots
mark :: Pointers a => Set Ptr -> Heap a -> Set Ptr
mark roots heap =
    let (gr, toPtr, toVertex) = toGraph heap
    in Set.fromList $ map toPtr
                    $ concatMap Foldable.toList
                    $ Graph.dfs gr (map toVertex (Set.toList roots))

-- | Given a set of reachable pointers, remove all unreachable pointers
--
-- Entries from the prelude are never collected (are always considered to
-- be reachable).
--
-- Returns the new heap and the set of removed pointers
sweep :: Show a => Set Ptr -> Heap a -> (Heap a, Set Ptr)
sweep reachable (Heap next hp) = (
      Heap next $ Map.filterWithKey (\ptr _a -> isReachable ptr) hp
    , Set.filter (not . isReachable) $ Map.keysSet hp
    )
  where
    isReachable :: Ptr -> Bool
    isReachable (Ptr Nothing _) = True
    isReachable ptr             = ptr `Set.member` reachable

-- | Mark-and-sweep garbage collection given a set of roots
--
-- Returns the new heap as well as the set of removed pointers
markAndSweep :: (Pointers a, Show a) => Set Ptr -> Heap a -> (Heap a, Set Ptr)
markAndSweep roots hp = sweep (mark roots hp) hp

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | Build an explicit graph representation of the heap
toGraph :: forall a. Pointers a
        => Heap a -> (Graph, Graph.Vertex -> Ptr, Ptr -> Graph.Vertex)
toGraph (Heap _ hp) =
    let (graph, f, g) = Graph.graphFromEdges edges
    in ( graph
       , \v -> case f v of ((), ptr, _ptrs) -> ptr
       , fromJust . g
       )
  where
    edges :: [((), Ptr, [Ptr])]
    edges = map mkEdge (Map.toList hp)

    mkEdge :: (Ptr, a) -> ((), Ptr, [Ptr])
    mkEdge (ptr, a) = ((), ptr, Set.toList (pointers a))
