  module Main (main) where

import Control.Monad

import CBN.Options
import CBN.Parser
import CBN.Trace
import CBN.Trace.HeapGraph  as Trace.HeapGraph
import CBN.Trace.JavaScript as Trace.JavaScript
import CBN.Trace.Textual    as Trace.Textual
import CBN.Trace.Graph      as Trace.Graph

main :: IO ()
main = do
    Options{..} <- getOptions
    input <- parseIO optionsInput parseModule =<< readFile optionsInput
    let trace = summarize optionsSummarize $ traceTerm optionsGC input
    when optionsShowTrace $      
      Trace.Textual.renderIO trace
    forM_ optionsJsOutput $ \file ->
      writeFile file $ Trace.JavaScript.render optionsJsName optionsGraphOutput trace
    forM_ optionsGraphOutput $ \file ->
      writeFile file $ Trace.Graph.render trace
    forM_ optionsGraphOutput $ toGraphFiles trace
