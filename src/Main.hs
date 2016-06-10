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
ex3 = [term| let length = \xs ->
               case xs of {
                   Nil        -> 0
                 ; Cons x xs' -> add 1 (length xs')
                 }
             in length (Cons Unit (Cons Unit (Cons Unit Nil)))
           |]

{-------------------------------------------------------------------------------
  Main
-------------------------------------------------------------------------------}

main :: IO ()
main = putStrLn $ take 10000 $ trace ex3
