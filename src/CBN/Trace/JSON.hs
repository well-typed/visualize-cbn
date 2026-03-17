{-# LANGUAGE OverloadedStrings #-}
module CBN.Trace.JSON (render) where

import Data.Aeson ((.=), object, encode)
import qualified Data.Aeson as Aeson
import Data.Maybe (listToMaybe)
import Data.Set (Set)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as Map
import qualified Data.Set as Set

import CBN.Eval
import CBN.Free () -- for Pointers instances on Term
import CBN.Heap
import CBN.Language
import CBN.Pretty
import CBN.Trace
import CBN.Util.Doc.Rendered.String (toString)
import qualified CBN.Util.Doc          as Doc
import qualified CBN.Util.Doc.Rendered as Rendered

render :: String -> Trace -> BL.ByteString
render source = encode . wrap . go 0
  where
    wrap frames = object [
        "source" .= source
      , "frames" .= frames
      ]

    go :: Int -> Trace -> [Aeson.Value]
    go n (Trace (hp, e) c) =
      let frame affected focus stepType desc =
            mkFrame n hp e affected focus stepType desc
      in case c of
        TraceWHNF _            -> [frame Set.empty Nothing "whnf" Nothing]
        TraceStuck err         -> [frame Set.empty Nothing "stuck" (Just $ "error: " ++ err)]
        TraceStopped           -> [frame Set.empty Nothing "stopped" Nothing]
        TraceStep d tr'        -> frame Set.empty (getFocus d) (getStepType d) (Just $ prettyText d)
                                    : go (n + 1) tr'
        TraceGC ps tr'         -> frame ps Nothing "gc" Nothing
                                    : go (n + 1) tr'
        TraceSelThunk _ ps tr' -> frame ps Nothing "selector" Nothing
                                    : go (n + 1) tr'
        TraceInline ps tr'     -> frame ps Nothing "inline" Nothing
                                    : go (n + 1) tr'

mkFrame :: Int -> Heap Term -> Term -> Set Ptr -> Maybe Ptr -> String -> Maybe String -> Aeson.Value
mkFrame n hp e affected focus stepType desc =
  object [
      "step"         .= n
    , "heap"         .= map (mkEntry affected focus) (Map.toList (heapEntries hp))
    , "term"         .= prettyText e
    , "termRefs"     .= map ptrToName (Set.toList (pointers e))
    , "stepType"     .= stepType
    , "description"  .= desc
    , "focus"        .= fmap ptrToName focus
    , "affectedPtrs" .= map ptrToName (Set.toList affected)
    ]

mkEntry :: Set Ptr -> Maybe Ptr -> (Ptr, Term) -> Aeson.Value
mkEntry affected focus (ptr, term) =
  object [
      "name"  .= ptrToName ptr
    , "expr"  .= prettyText term
    , "refs"  .= map ptrToName (Set.toList (pointers term))
    , "state" .= st
    ]
  where
    st :: String
    st | ptr `Set.member` affected = "affected"
       | Just ptr == focus         = "focus"
       | otherwise                 = "normal"

getFocus :: DescriptionWithContext -> Maybe Ptr
getFocus (DescriptionWithContext _ ctxt) = listToMaybe (reverse ctxt)

getStepType :: DescriptionWithContext -> String
getStepType (DescriptionWithContext d _) = case d of
  StepAlloc        -> "alloc"
  StepBeta         -> "beta"
  StepApply _      -> "apply"
  StepDelta _      -> "delta"
  StepMatch _      -> "match"
  StepIf _         -> "if"
  StepSeq          -> "seq"
  StepAllocConArgs -> "allocConArgs"

-- | Render a ToDoc value as plain text
prettyText :: ToDoc a => a -> String
prettyText = toString . Doc.render (\r -> Rendered.width r <= 80) . toDoc

-- | Convert a pointer to its display name
ptrToName :: Ptr -> String
ptrToName (Ptr Nothing  (Just s))  = s
ptrToName (Ptr (Just n) (Just s))  = s ++ "_" ++ show n
ptrToName (Ptr (Just n) Nothing)   = show n
ptrToName (Ptr Nothing  Nothing)   = error "invalid pointer"
