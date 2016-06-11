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

ex1, ex2, ex3, ex4, ex5, ex6, ex7 :: Term
ex1 = [term| \x -> x |]
ex2 = [term| (\x -> x x) (\x -> x x) |]
ex3 = [term| let length = \xs ->
               case xs of {
                   Nil        -> 0
                 ; Cons x xs' -> add 1 (length xs')
                 }
             in length (Cons Unit (Cons Unit (Cons Unit Nil)))
           |]
ex4 = [term| \x -> \y -> y |]
ex5 = [term| case Nil of { Nil -> case Nil of { Nil -> 0 } } |]
ex6 = [term| let x = 0 in let y = 1 in x |]
ex7 = [term| let x = 0 in case x of { Nil -> 1 } |]

exLengthEnumFromTo :: Term
exLengthEnumFromTo = [term|
    let if = \b -> \t -> \f -> case b of { True -> t ; False -> f } in
    let enumFromTo = \n -> \m -> if (le n m) (Cons n (enumFromTo (add n 1) m)) Nil in
    0
  |]

{-------------------------------------------------------------------------------
  Main
-------------------------------------------------------------------------------}

main :: IO ()
main = putStrLn $ take 10000 $ trace exLengthEnumFromTo
