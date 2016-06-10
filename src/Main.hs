{-# LANGUAGE QuasiQuotes #-}
module Main where

import CBN.Language
import CBN.Heap
import CBN.Eval
import CBN.Parser
import CBN.Pretty

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

ex1, ex2, ex3 :: Term
ex1 = [term| \x -> x |]
ex2 = [term| (\x -> x x) (\x -> x x) |]
ex3 = [term| \xs -> case xs of {
                 Nil        -> Zero
               ; Cons x xs' -> Succ 
               }
           |]

{-------------------------------------------------------------------------------
  Main
-------------------------------------------------------------------------------}

main :: IO ()
main = print $ pretty ex3
