module CBN.InlineHeap (inlineHeap) where

import Data.Bifunctor
import Data.Set (Set)
import Data.List (partition)

import qualified Data.Map as Map
import qualified Data.Set as Set

import CBN.Heap
import CBN.Language
import CBN.Subst

{-------------------------------------------------------------------------------
  Simplification

  We only heap allocate non-simple terms, to keep things readable. However,
  during evaluation previously heap-allocated terms may /become/ simple. If
  simplification is enabled, we then "remove" these from the heap by inlining
  them.
-------------------------------------------------------------------------------}

inlineHeap :: Heap Term -> Term -> (Heap Term, Term, Set Ptr)
inlineHeap (Heap next entries) e = (
       Heap {
           heapNextAvailable = next
         , heapEntries       = Map.fromList $
                                 map (second (substPtrs toInline)) toKeep
         }
    , substPtrs toInline e
    , Set.fromList $ map fst toInline
    )
  where
    toInline, toKeep :: [(Ptr, Term)]
    (toInline, toKeep) = partition (canInline . snd) (Map.toList entries)

canInline :: Term -> Bool
canInline TVar{}  = False
canInline TApp{}  = False
canInline TLam{}  = False
canInline TLet{}  = False
canInline TPtr{}  = True
canInline TCase{} = False
canInline TIf{}   = False
canInline TSeq{}  = False
canInline (TPrim (PrimApp _ es)) = null es -- if args then is application
canInline (TCon  (ConApp _ es))  = all canInline es