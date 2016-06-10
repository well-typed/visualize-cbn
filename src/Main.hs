{-# LANGUAGE QuasiQuotes #-}
module Main where

import Data.List (intercalate)
import qualified Data.Map as Map

import CBN.Language
import CBN.Heap
import CBN.Eval
import CBN.Parser

{-------------------------------------------------------------------------------
  Show instances
-------------------------------------------------------------------------------}

instance Show Con where
  show (Con c) = c

instance Show Var where
  show (Var x) = x

type Count = Int

instance Show Pat where
  show (Pat c xs) = intercalate " " (show c:map show xs)

instance Show Match where
  showsPrec p (Match pat e) = showsPrec 0 pat . (" -> " ++) . showsPrec p e

instance Show Term where
  showsPrec p = \case
      TVar x     -> showsPrec 0 x
      TApp e1 e2 -> bracketIf (p > precApp) $
                        showsPrec precApp e1
                      . (' ':)
                      . showsPrec precApp e2
      TLam x e   -> bracketIf (p > precLam) $
                        ('\\':)
                      . showsPrec 0 x
                      . (" -> " ++)
                      . showsPrec precLam e
      TPtr ptr   -> showsPrec 0 ptr
      TCon c es  -> bracketIf (p > precCon) $
                        showsPrec 0 c
                      . foldr (.) id (map (\e -> (' ':) . showsPrec precCon e) es)
      TPat e ms  -> bracketIf (p > precPat) $
                        ("case " ++)
                      . (showsPrec 0 e)
                      . (" of {" ++)
                      . foldr (.) id (map (\m -> (' ':) . showsPrec 0 m) ms)
                      . (" }" ++)
    where
      precApp, precCon, precPat, precLam :: Int
      precApp = 4
      precCon = 3
      precPat = 2
      precLam = 1

instance Show Value where
  show = show . valueToTerm

instance Show Ptr where
  show (Ptr n) = show n

instance Show a => Show (Heap a) where
  show (Heap mp) = show (Map.toList mp)

bracketIf :: Bool -> ShowS -> ShowS
bracketIf False s = s
bracketIf True  s = ('(':) . s . (')':)

{-------------------------------------------------------------------------------
  Textual execution
-------------------------------------------------------------------------------}

trace :: Term -> String
trace = go emptyHeap
  where
    go :: Heap Term -> Term -> String
    go hp e = show (hp, e) ++ "\n"
           ++ case step hp e of
                Step hp' e' -> go hp' e'
                WHNF _      -> "(whnf)"
                Stuck err   -> "(stuck: " ++ err ++ ")"

{-------------------------------------------------------------------------------
  Examples
-------------------------------------------------------------------------------}

ex1, ex2 :: Term
ex1 = [term| \x -> x |]
ex2 = [term| (\x -> x x) (\x -> x x) |]


{-
ex3 :: (Heap, Term)
ex3 = (hp1, undefined)
  where
    (hp1, ptrLen) = alloc hp0 (TLam "xs" (TPat (TVar "xs") [))
    hp0 = emptyHeap
-}

{-------------------------------------------------------------------------------
  Main
-------------------------------------------------------------------------------}

main :: IO ()
main = putStrLn "Hello, Haskell!"
