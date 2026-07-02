  module Main (main) where

import Control.Monad
import qualified Data.ByteString.Lazy as BL

import CBN.Options
import CBN.Parser
import CBN.Trace
import CBN.Trace.HeapGraph  as Trace.HeapGraph
import CBN.Trace.JSON       as Trace.JSON
import CBN.Trace.JavaScript as Trace.JavaScript
import CBN.Trace.Textual    as Trace.Textual
import CBN.Trace.Graph      as Trace.Graph

main :: IO ()
main = do
    Options{..} <- getOptions
    sourceCode <- readFile optionsInput
    input <- parseIO optionsInput parseModule sourceCode
    let trace = summarize optionsSummarize $
                  traceTerm
                    optionsGC
                    optionsInlineHeap
                    optionsSelThunkOpt
                    input
    when optionsShowTrace $
      Trace.Textual.renderIO optionsDisableAnsi trace
    forM_ optionsJsOutput $ \file ->
      writeFile file $ Trace.JavaScript.render optionsJsName optionsGraphOutput trace
    forM_ optionsGraphOutput $ \file ->
      writeFile file $ Trace.Graph.render trace
    forM_ optionsGraphOutput $ toGraphFiles trace
    forM_ optionsJsonOutput $ \file ->
      BL.writeFile file $ Trace.JSON.render sourceCode trace
