module CBN.Closure (toClosureGraph, Closure(..), Id) where

import Data.Maybe (fromJust)
import Data.Graph as Graph
import Control.Monad.State
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Tree as Tree

import CBN.Language
import CBN.Heap
import CBN.Trace

-- Some Terms have a heap Pointer, but not an explicit CBN.Heap.Ptr.
-- For example the closure `Cons 1 Nil` has a Pointer to `1`
-- and to `Nil`. All values are assumed boxed.

-- | Id is the unique id of each node.
data Id = Id Ptr Int
 deriving (Eq, Ord)

-- | The Id for objects with an explicit Heap.Ptr.
defaultId :: Ptr -> Id
defaultId p = Id p 0

-- | Each Closure has a Header and some edges (the Payload).
data Closure =
    ErrorClosure String
  | FunClosure Term [Ptr]
  | ConClosure Con [Term]

  -- IndirectionClosures can be ignored while looking at the graph.
  -- An idirection of a thunk is a thunk and an indirection to a
  -- whnf is whnf. Indirections are not yet eliminated because they can
  -- have loops, so this task is not trivial.
  | IndirectionClosure Ptr

  -- ThunkClosure also includes Closures for function application.
  -- This could be improved in the future. See also:
  -- https://ghc.haskell.org/trac/ghc/wiki/Commentary/Rts/Storage/HeapObjects#Genericapplication
  | ThunkClosure Term [Ptr]
  | PrimClosure Prim [Term]
  deriving (Show)

-- | Eventually all edges are heap pointers. The distinction here is made
-- because related terms don`t have an assigned Id when this function is used.
extractEdges :: Closure -> ([Ptr], [Term])
extractEdges cl = case cl of
  ErrorClosure _ -> ([], [])
  FunClosure _ ls -> (ls, [])
  ConClosure _ ls -> ([], ls)
  IndirectionClosure ptr -> ([ptr], [])
  ThunkClosure _ ls -> (ls, [])
  PrimClosure _ ls -> ([], ls)

thunk :: Term -> Closure
thunk term = ThunkClosure term $ Set.toList $ pointers term

-- Heap could be used in the future to eliminate Indirections.

toClosure :: (Heap Term, Term) -> Closure
toClosure (heap, term) = case term of
  TVar (Var x) -> ErrorClosure $ "free variable " ++ show x
  TLam _ _ -> FunClosure term ls
    where ls = Set.toList $ pointers term
  TCon (ConApp con terms) -> ConClosure con terms
  TPtr ptr -> IndirectionClosure ptr
  TPrim (PrimApp p es) -> PrimClosure p es
  TLet _ _ _ -> thunk term
  TApp _ _ -> thunk term
  TCase _ _ -> thunk term
  TIf _ _ _ -> thunk term
  TSeq _ _ -> thunk term

-- | Build a representation of the heap, including terms that don`t
-- have an explicit Heap.Ptr.
toClosureGraph :: (Heap Term, Term) -> (Graph, Graph.Vertex ->
  (Closure, Id, [Id]), Id -> Graph.Vertex)
toClosureGraph (heap@(Heap _ hp), term) =
  let (graph, f, g) = Graph.graphFromEdges edges
  in (graph, f , fromJust . g)
  where
    main = (Ptr Nothing (Just "main"), term)

    edges :: [(Closure, Id, [Id])]
    edges = concatMap mkTree $ main : Map.toList hp

    -- If we ignore Heap.Ptrs, each heap term, defines a tree of other reachable
    -- terms.
    mkTree :: (Ptr, Term) -> [(Closure, Id, [Id])]
    mkTree (ptr, term) = Tree.flatten $ addInternalEdges tree
      where
        identify :: State Int Id
        identify = do
          myid <- get
          put $ myid + 1
          return $ Id ptr myid

        addInternalEdges :: Tree (Id, (Closure, [Id])) -> Tree (Closure, Id, [Id])
        addInternalEdges (Node (myid, (cl, ids)) subTrees) = Node (cl, myid, newIds) cont
          where
            childIds = fmap (fst . Tree.rootLabel) subTrees
            newIds = ids ++ childIds
            cont = map addInternalEdges subTrees

        tree :: Tree (Id, (Closure, [Id]))
        tree = evalState (Tree.unfoldTreeM f term) 0

        -- The [Id] here contains only the pointer ids and
        -- Not the Ids of the same Tree, as those are not assigned yet.
        -- They are added  at `addInternalEdges`, after the creation of the whole tree.

        f :: Term -> State Int ((Id, (Closure, [Id])), [Term])
        f term = do
          myid <- identify
          let closure = toClosure (heap, term)
          let (ptrs, terms) = extractEdges closure
          return ((myid, (closure, map defaultId ptrs)), terms)
