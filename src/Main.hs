{-# LANGUAGE QuasiQuotes #-}
module Main where

import CBN.Heap
import CBN.Language
import CBN.Parser
import CBN.Trace
import CBN.Trace.Textual as Trace.Textual
import CBN.Trace.HTML    as Trace.HTML

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
main = putStrLn $ Trace.HTML.toJS "lengthEnumFromTo"
                $ limitSteps 1000
                $ trace (prelude, exLengthEnumFromTo)
