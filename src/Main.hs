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

trace :: (Heap Term, Term) -> String
trace = go 0
  where
    go :: Int -> (Heap Term, Term) -> String
    go n (hp, e) =
         "** " ++ show n  ++ "\n"
      ++ show (pretty hp) ++ "\n"
      ++ show (pretty e)  ++ "\n"
      ++ "\n"
      ++ case step hp e of
           Step hp' e' -> go (n + 1) (hp', e')
           WHNF _      -> "(whnf)"
           Stuck err   -> "(stuck: " ++ err ++ ")"

{-------------------------------------------------------------------------------
  Examples
-------------------------------------------------------------------------------}

exLengthEnumFromTo :: Term
exLengthEnumFromTo = [term| @length (@enumFromTo 1 3) |]

prelude :: Heap Term
prelude = initHeap [
      ("if", [term|
         \b -> \t -> \f -> case b of { True -> t ; False -> f }
      |])
    , ("enumFromTo", [term|
         \n -> \m -> @if (le n m) (Cons n (@enumFromTo (add n 1) m)) Nil
      |])
    , ("length", [term|
         \xs -> case xs of { Nil -> 0 ; Cons x xs' -> add 1 (@length xs') }
      |])
    ]

{-------------------------------------------------------------------------------
  Main
-------------------------------------------------------------------------------}

main :: IO ()
main = putStrLn $ take 100000 $ trace (prelude, exLengthEnumFromTo)
