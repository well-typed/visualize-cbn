module CBN.Trace.Textual (renderIO) where

import Data.List (intersperse)
import Data.Set (Set)
import qualified Data.Set as Set

import CBN.Heap
import CBN.Trace
import CBN.Pretty
import qualified CBN.Util.Doc               as Doc
import qualified CBN.Util.Doc.Rendered      as Rendered
import qualified CBN.Util.Doc.Rendered.ANSI as ANSI

renderIO :: Trace -> IO ()
renderIO = go 0
  where
    go :: Int -> Trace -> IO ()
    go n (Trace (hp, e) c) = do
      case c of
        TraceWHNF _      -> mkFrame Set.empty (putStr $ "whnf")
        TraceStuck err   -> mkFrame Set.empty (putStr $ "stuck: " ++ err)
        TraceStopped     -> mkFrame Set.empty (putStr $ "stopped")
        TraceStep d tr'  -> mkFrame Set.empty (pretty d)  >> go (n + 1) tr'
        TraceGC  ps tr'  -> mkFrame ps        (goPtrs ps) >> go (n + 1) tr'
      where
        mkFrame :: Set Ptr -> IO () -> IO ()
        mkFrame garbage msg = do
          putStrLn $ "** " ++ show n
          pretty (heapToDoc garbage hp) ; putChar '\n'
          pretty e ; putChar '\n'
          putChar '\n'
          putStr "(" ; msg ; putStrLn ")\n"

    goPtrs :: Set Ptr -> IO ()
    goPtrs ps = do
      putStr "collecting "
      sequence_ . intersperse (putStr ", ") . map pretty $ Set.toList ps

    pretty :: ToDoc a => a -> IO ()
    pretty = ANSI.write . Doc.render (\r -> Rendered.width r <= 80) . toDoc
