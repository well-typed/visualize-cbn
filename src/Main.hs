{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Map (Map)
import Data.String (IsString)
import qualified Data.Map as Map

{-------------------------------------------------------------------------------
  Variables
-------------------------------------------------------------------------------}

newtype Var = Var String
  deriving (Eq, Ord, IsString)

instance Show Var where
  show (Var x) = x

type Count = Int

class Free a where
  free :: a -> Map Var Count

instance Free Var where
  free x = Map.singleton x 1

instance Free a => Free [a] where
  free = Map.unionsWith (+) . map free

{-------------------------------------------------------------------------------
  Terms
-------------------------------------------------------------------------------}

data Term =
    TVar Var
  | TApp Term Term
  | TLam Var Term
  | TPtr Ptr

instance Show Term where
  showsPrec p = \case
      TVar x     -> showsPrec 0 x
      TApp e1 e2 -> bracketIf (p > precApp) $ showsPrec precApp e1
                                            . (' ':)
                                            . showsPrec precApp e2
      TLam x e   -> bracketIf (p > precLam) $ ('\\':)
                                            . showsPrec 0 x
                                            . (". " ++)
                                            . showsPrec precLam e
      TPtr ptr   -> showsPrec 0 ptr
    where
      bracketIf :: Bool -> ShowS -> ShowS
      bracketIf False s = s
      bracketIf True  s = ('(':) . s . (')':)

      precApp, precLam :: Int
      precApp = 2
      precLam = 1

instance Free Term where
  free (TVar x)     = free x
  free (TApp e1 e2) = free [e1, e2]
  free (TLam x e)   = Map.delete x $ free e
  free (TPtr _)     = Map.empty

{-------------------------------------------------------------------------------
  Values
-------------------------------------------------------------------------------}

data Value =
    VLam Var Term
  deriving (Show)

{-------------------------------------------------------------------------------
  Heap
-------------------------------------------------------------------------------}

newtype Ptr = HeapPtr Int
  deriving (Eq, Ord)

instance Show Ptr where
  show (HeapPtr n) = show n

newtype Heap = Heap (Map Ptr Term)

instance Show Heap where
  show (Heap mp) = show (Map.toList mp)

emptyHeap :: Heap
emptyHeap = Heap Map.empty

alloc :: Heap -> Term -> (Heap, Ptr)
alloc (Heap hp) (TPtr n) = (Heap hp, n)
alloc (Heap hp) e        = (Heap (Map.insert ptr e hp), ptr)
  where
    ptr :: Ptr
    ptr = HeapPtr (Map.size hp)

deref :: Heap -> Ptr -> Term
deref (Heap hp) ptr = hp Map.! ptr

mutate :: Heap -> Ptr -> Term -> Heap
mutate (Heap hp) ptr term = Heap (Map.insert ptr term hp)

substPtr :: Var -> Ptr -> Term -> Term
substPtr x ptr = go
  where
    go :: Term -> Term
    go (TVar x')    = if x == x' then TPtr ptr else TVar x'
    go (TApp e1 e2) = TApp (go e1) (go e2)
    go (TLam x' e)  = TLam x' (if x == x' then e else go e)
    go (TPtr ptr')  = TPtr ptr'

{-------------------------------------------------------------------------------
  Evaluation
-------------------------------------------------------------------------------}

data Step = Step Heap Term | WHNF Value | Stuck

step :: Heap -> Term -> Step
step _  (TVar _)     = Stuck
step _  (TLam x e)   = WHNF $ VLam x e
step hp (TPtr ptr)   = case step hp (deref hp ptr) of
                         Step hp' e' -> Step (mutate hp' ptr e') (TPtr ptr)
                         Stuck       -> Stuck
                         WHNF val    -> WHNF val
step hp (TApp e1 e2) = case step hp e1 of
                         Step hp' e1'      -> Step hp' $ TApp e1' e2
                         Stuck             -> Stuck
                         WHNF (VLam x e1') -> let (hp', ptr) = alloc hp e2
                                              in Step hp' $ substPtr x ptr e1'

{-------------------------------------------------------------------------------
  Textual execution
-------------------------------------------------------------------------------}

trace :: Term -> String
trace = go emptyHeap
  where
    go :: Heap -> Term -> String
    go hp e = show (hp, e) ++ "\n"
           ++ case step hp e of
                Step hp' e' -> go hp' e'
                WHNF _      -> "(whnf)"
                Stuck       -> "(stuck)"

{-------------------------------------------------------------------------------
  Examples
-------------------------------------------------------------------------------}

ex1 :: Term
ex1 =      (TLam "x" (TApp (TVar "x") (TVar "x")))
    `TApp` (TLam "x" (TApp (TVar "x") (TVar "x")))

ex2 :: Term
ex2 = TLam "x" (TVar "x")

{-------------------------------------------------------------------------------
  Main
-------------------------------------------------------------------------------}

main :: IO ()
main = putStrLn "Hello, Haskell!"
