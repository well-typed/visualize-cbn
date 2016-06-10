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
trace = go 0 emptyHeap
  where
    go :: Int -> Heap Term -> Term -> String
    go n hp e =
         "** " ++ show n  ++ "\n"
      ++ show (pretty hp) ++ "\n"
      ++ show (pretty e)  ++ "\n"
      ++ "\n"
      ++ case step hp e of
           Step hp' e' -> go (n + 1) hp' e'
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
main = putStrLn $ take 500 $ trace ex2
