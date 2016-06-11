{-# LANGUAGE QuasiQuotes #-}
module Main where

import Control.Monad

import CBN.Heap
import CBN.Language
import CBN.Options
import CBN.Parser
import CBN.Trace
import CBN.Trace.JavaScript as Trace.JavaScript
import CBN.Trace.Textual    as Trace.Textual

{-------------------------------------------------------------------------------
  Prelude
-------------------------------------------------------------------------------}

prelude :: Heap Term
prelude = initHeap [
      ("enumFromTo", [term|
         \n -> \m -> if le n m then Cons n (@enumFromTo (add n 1) m)
                               else Nil
      |])
    , ("length", [term|
         \xs -> case xs of { Nil -> 0 ; Cons x xs' -> add 1 (@length xs') }
      |])
    ]

{-------------------------------------------------------------------------------
  Main
-------------------------------------------------------------------------------}

main :: IO ()
main = do
    Options{..} <- getOptions
    input <- parseIO optionsInput parseTerm =<< readFile optionsInput
    let trace = summarize optionsSummarize
              $ traceTerm optionsGC (prelude, input)
    when optionsShowTrace $
      putStrLn $ Trace.Textual.render trace
    forM_ optionsJsOutput $ \file ->
      writeFile file $ Trace.JavaScript.render optionsJsName trace
