module CBN.Options (
    Options(..)
  , getOptions
  ) where

import Options.Applicative
import CBN.Trace

data Options = Options {
      optionsInput       :: FilePath
    , optionsShowTrace   :: Bool
    , optionsGC          :: Bool
    , optionsSummarize   :: SummarizeOptions
    , optionsJsOutput    :: Maybe FilePath
    , optionsJsName      :: String
    , optionsGraphOutput :: Maybe FilePath
    , optionsGraphTermsOutput :: Maybe FilePath
    }
  deriving (Show)

getOptions :: IO Options
getOptions = execParser $ info (helper <*> parseOptions) fullDesc

parseOptions :: Parser Options
parseOptions = Options
    <$> (strOption $ mconcat [
             short 'i'
           , help "Input file"
           , metavar "INPUT-FILE"
           ])
    <*> (switch $ mconcat [
             long "show-trace"
           , help "Write trace to console"
           ])
    <*> (switch $ mconcat [
             long "gc"
           , help "GC after each step"
           ])
    <*> parseSummarizeOptions
    <*> (optional . strOption $ mconcat [
             long "javascript"
           , help "Generate JavaScript output"
           , metavar "JS-FILE"
           ])
    <*> (strOption $ mconcat [
             long "javascript-function"
           , help "Function name prefix in the JavaScript output"
           , metavar "JS-NAME"
           , showDefault
           , value "cbn"
           ])
    <*> (optional . strOption $ mconcat [
             long "graph"
           , help "Generate a graph output in dot format"
           , metavar "GRAPH-FILE"
           ])
    <*> (optional . strOption $ mconcat [
             long "heap-graph"
           , help "Generate one graph representation file for each step"
           , metavar "PATH/FILES-PREFIX"
           ])

parseSummarizeOptions :: Parser SummarizeOptions
parseSummarizeOptions = SummarizeOptions
    <$> (switch $ mconcat [
             long "collapse-beta"
           , help "Collapse adjacent beta steps"
           ])
    <*> (option auto $ mconcat [
             long "max-num-steps"
           , help "Maximum number of steps"
           , showDefault
           , value 1000
           , metavar "N"
           ])
    <*> (switch $ mconcat [
             long "hide-prelude"
           , help "Hide the prelude from the help"
           ])
    <*> (many $ option str $ mconcat [
             long "hide-term"
           , help "Hide specific term from the prelude (can be used multiple times)"
           ])
    <*> (switch $ mconcat [
             long "hide-gc"
           , help "Hide GC steps"
           ])
