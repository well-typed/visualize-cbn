module CBN.Trace.Textual (renderIO) where

import Data.List (intersperse)
import Data.Maybe (listToMaybe)
import Data.Set (Set)

import qualified Data.Set as Set

import CBN.Eval
import CBN.Heap
import CBN.Pretty
import CBN.Trace

import qualified CBN.Util.Doc                 as Doc
import qualified CBN.Util.Doc.Rendered        as Rendered
import qualified CBN.Util.Doc.Rendered.ANSI   as ANSI
import qualified CBN.Util.Doc.Rendered.String as String

renderIO :: Bool -> Trace -> IO ()
renderIO disableAnsi = go 0
  where
    go :: Int -> Trace -> IO ()
    go n (Trace (hp, e) c) = do
      case c of
        TraceWHNF _          -> mkFrame Set.empty Nothing (putStr $ "whnf")
        TraceStuck err       -> mkFrame Set.empty Nothing (putStr $ "stuck: " ++ err)
        TraceStopped         -> mkFrame Set.empty Nothing (putStr $ "stopped")
        TraceStep d tr'      -> mkFrame Set.empty (mkFocus d) (pretty d)  >> go (n + 1) tr'
        TraceGC ps tr'       -> mkFrame ps Nothing (ptrs "collecting" ps) >> go (n + 1) tr'
        TraceSelThunk ps tr' -> mkFrame ps Nothing (ptrs "apply selectors" ps) >> go (n + 1) tr'
        TraceInline ps tr'   -> mkFrame ps Nothing (ptrs "inlining" ps) >> go (n + 1) tr'
      where
        mkFrame :: Set Ptr -> Maybe Ptr -> IO () -> IO ()
        mkFrame garbage focus msg = do
          putStrLn $ "** " ++ show n
          pretty (heapToDoc garbage focus hp) ; putChar '\n'
          pretty e ; putChar '\n'
          putChar '\n'
          putStr "(" ; msg ; putStrLn ")\n"

    ptrs :: String -> Set Ptr -> IO ()
    ptrs label ps = do
      putStr (label ++ " ")
      sequence_ . intersperse (putStr ", ") . map pretty $ Set.toList ps

    pretty :: ToDoc a => a -> IO ()
    pretty =
          ( if disableAnsi
              then putStr . String.toString
              else ANSI.write
          )
        . Doc.render (\r -> Rendered.width r <= 80)
        . toDoc

    mkFocus :: DescriptionWithContext -> Maybe Ptr
    mkFocus (DescriptionWithContext _ ctxt) = listToMaybe (reverse ctxt)
