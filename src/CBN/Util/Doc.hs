{-# LANGUAGE CPP #-}
module CBN.Util.Doc (
    Table
  , Doc -- opaque
  , doc
  , table
  , choice
  , style
  , stack
  , render
  , renderAll
  ) where

import Data.Default
import Data.Foldable (asum)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Semigroup as Sem

import CBN.Util.Doc.Rendered (Table, Rendered)
import qualified CBN.Util.Doc.Rendered as Rendered

{-------------------------------------------------------------------------------
  Documents
-------------------------------------------------------------------------------}

-- | Abstract description of a document
--
-- The two type parameters are the type of the styling applied to the document
-- and the type of primitive documents.
data Doc st a =
    -- | Primitive document
    Doc a

    -- | Append two documents
  | Append (Doc st a) (Doc st a)

    -- | Align a bunch of documents like in a table
    --
    -- Outermost list: rows; innermost list: columns
  | Table [[Doc st a]]

    -- | Alternative renderings
  | Choice [Doc st a]

    -- | Apply style
  | Style (st -> st) (Doc st a)

instance Sem.Semigroup (Doc st a) where
    (<>) = Append

-- | The standard monoidal corresponds to horizontal composition
instance Monoid a => Monoid (Doc st a) where
  mempty  = Doc mempty
#if !(MIN_VERSION_base(4,11,0))
  mappend = (<>)
#endif

-- | Primitive document
doc :: a -> Doc st a
doc = Doc

-- | Table of documents
table :: [[Doc st a]] -> Doc st a
table = Table

-- | Multiple alternative renderings
choice :: [Doc st a] -> Doc st a
choice = Choice

-- | Apply style
style :: (st -> st) -> Doc st a -> Doc st a
style = Style

-- | Vertical composition of documents
stack :: [Doc st a] -> Doc st a
stack = Table . map (:[])

{-------------------------------------------------------------------------------
  Rendering
-------------------------------------------------------------------------------}

-- | Compute all possible ways to render this document
renderAll :: Default st => Doc st String -> [Rendered st]
renderAll (Doc str)      = return $ Rendered.fromString str
renderAll (Choice ds)    = asum (map renderAll ds)
renderAll (Append d1 d2) = mappend <$> renderAll d1 <*> renderAll d2
renderAll (Table dss)    = Rendered.table <$> mapM (mapM renderAll) dss
renderAll (Style st d)   = fmap st <$> renderAll d

render :: Default st => (Rendered st -> Bool) -> Doc st String -> Rendered st
render p d = fromMaybe (head $ renderAll d) (find p $ renderAll d)
