module Main (main) where

import Control.Monad

import CBN.Options
import CBN.Parser
import CBN.Trace
import CBN.Trace.JavaScript as Trace.JavaScript
import CBN.Trace.Textual    as Trace.Textual

main :: IO ()
main = do
    Options{..} <- getOptions
    input <- parseIO optionsInput parseModule =<< readFile optionsInput
    let trace = summarize optionsSummarize $ traceTerm optionsGC input
    when optionsShowTrace $
      putStrLn $ Trace.Textual.render trace
    forM_ optionsJsOutput $ \file ->
      writeFile file $ Trace.JavaScript.render optionsJsName trace
