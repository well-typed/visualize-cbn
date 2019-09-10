{-# LANGUAGE CPP #-}
-- | Rendered documents
--
-- Intended for qualified import
--
-- > import CBN.Util.Doc.Rendered (Rendered)
-- > import qualified CBN.Util.Doc.Rendered as Rendered
module CBN.Util.Doc.Rendered (
    Table
  , Rendered(..)
  , fromString
  , table
    -- * Auxiliary
  , rTrim
  ) where

import Data.Bifunctor
import Data.Default
import Data.List (transpose)
import Data.Maybe (isNothing)
import Data.Semigroup as Sem

-- | A table in rows-of-columns format
--
-- For example, every element of @Table Char@ is a 'String' (such as a line
-- in a rendered document); i.e.,
--
-- > [['a', 'b', 'c']]
--
-- corresponds to
--
-- > +---+---+---+
-- > | a | b | c |
-- > +---+---+---+
--
-- Conversely,
--
-- > [['a'],['b'],['c']]
--
-- corresponds to
--
-- > +---+
-- > | a |
-- > +---+
-- > | b |
-- > +---+
-- > | c |
-- > +---+
--
-- Finally, we have that
--
-- > [['a', 'b'], ['c', 'd']]
--
-- corresponds to
--
-- > +---+---+
-- > | a | b |
-- > +---+---+
-- > | c | d |
-- > +---+---+
type Table a = [[a]]

-- | Rendered document
--
-- This is parameterized by the style of each character.
--
-- INVARIANT: All lines must be of the same length ('rendered' is a rectangle).
data Rendered st = Rendered {
      width    :: Int
    , height   :: Int
    , rendered :: [[Maybe (st, Char)]] -- ^ 'Nothing' indicates padding
    }
    deriving (Show)

instance Functor Rendered where
  fmap f r = r { rendered = map (map (fmap (first f))) (rendered r) }

instance Sem.Semigroup (Rendered st) where
    (<>) = nestle

-- | Like the instance for 'Doc', this corresponds to horizontal composition
instance Monoid (Rendered st) where
  mempty  = empty
#if !(MIN_VERSION_base(4,11,0))
  mappend = (<>)
#endif
  mconcat = nestles

-- | Empty rendered documents
empty :: Rendered st
empty = Rendered {
      width    = 0
    , height   = 0
    , rendered = []
    }

-- | Construct rendered document from a string (possibly containing linebreaks)
fromString :: Default st => String -> Rendered st
fromString str = Rendered {
      width    = newWidth
    , height   = length ss
    , rendered = map (padTo newWidth . map (\c -> Just (def, c))) ss
    }
  where
    ss       = lines str
    newWidth = maximum $ map length ss

-- | Set the width of a rendered document (by padding where necessary)
setWidth :: Int -> Rendered st -> Rendered st
setWidth n r = Rendered {
      width    = newWidth
    , height   = height r
    , rendered = map (padTo newWidth) (rendered r)
    }
  where
    newWidth = max n (width r)

-- | Vertical composition of rendered documents
--
-- Vertical composition is straight-forward; we just have to make sure to
-- pad the documents.
stack :: Rendered st -> Rendered st -> Rendered st
stack r1 r2 = Rendered {
      width    = newWidth
    , height   = newHeight
    , rendered = map (padTo newWidth) (rendered r1 ++ rendered r2)
    }
  where
    newWidth  = max (width r1) (width r2)
    newHeight = height r1 + height r2

stacks :: [Rendered st] -> Rendered st
stacks = foldr stack empty

-- | Horizontal composition of rendered documents
--
-- Since we are dealing with source code, horizontal composition of documents
-- is somewhat peculiar. It will look like this:
--
-- >  +-----------+
-- >  |           |
-- >  |           |
-- >  |           |    +--------------+
-- >  |           |    |              |
-- >  +-----------+    |              |
-- >                   |              |
-- >                   |              |
-- >                   +--------------|
--
-- so that the last line of the first box lines up with the first line of the
-- second box. To see this, consider something like
--
-- >  +------------------+
-- >  | case xs of       |
-- >  |   Nil -> 0       |    +-----------------------+
-- >  |   Cons x xs' ->  |    | let xs'' = map foo xs |
-- >  +------------------+    | in bar xs''           |
-- >                          +-----------------------|
--
-- See also 'nestle', which can often result in a more natural layout
-- (but is less useful when rendering tables).
stagger :: Rendered st -> Rendered st -> Rendered st
stagger r1 r2
  | height r1 == 0 = r2
  | height r2 == 0 = r1
  | otherwise = Rendered {
        width    = newWidth
      , height   = newHeight
      , rendered = zipWith (++) (rendered r1 ++ padding  r1)
                                (padding  r2 ++ rendered r2)
      }
  where
    newWidth  = width  r1 + width  r2
    newHeight = height r1 + height r2 - 1 -- they overlap by one line
    padding r = replicate (newHeight - height r)
                          (replicate (width r) Nothing)

staggers :: [Rendered st] -> Rendered st
staggers = foldr stagger empty

-- | Like 'stagger', but reduce the horizontal space
--
-- >  +-----------+
-- >  |           |
-- >  |           |
-- >  |       +--------------+
-- >  |       |              |
-- >  +-------|              |
-- >          |              |
-- >          |              |
-- >          +--------------|
--
-- For example:
--
-- >  +-------------------+
-- >  | case xs of        |
-- >  |   Cons x xs' -> 0 |-----------+
-- >  |   Nil -> | case ys of         |
-- >  +----------|   Cons y ys' -> .. |
-- >             |   Nil        -> .. |
-- >             +--------------------|
--
-- This generally leads to a more natural layout then 'stagger'.
nestle :: Rendered st -> Rendered st -> Rendered st
nestle r1 r2
  | height r1 == 0 = r2
  | height r2 == 0 = r1
  | otherwise = Rendered {
        width    = newWidth
      , height   = newHeight
      , rendered = map (padTo newWidth) $ concat [
            init (rendered r1)
          , [lastLine_r1 ++ head (rendered r2)]
          , map (replicate lastWidth_r1 Nothing ++) (tail (rendered r2))
          ]
      }
  where
    newWidth     = max (lastWidth_r1 + width r2) (width r1)
    newHeight    = height r1 + height r2 - 1 -- they overlap by one line
    lastLine_r1  = rTrim $ last (rendered r1)
    lastWidth_r1 = length lastLine_r1

nestles :: [Rendered st] -> Rendered st
nestles = foldr nestle empty

-- | Render a table
--
-- A table must be rendered such that cells in the same column are lined up
-- horizontally; cells in a row must be lined up in the usual staggered
-- manner (see 'rStagger').
table :: forall st. Table (Rendered st) -> Rendered st
table rss =
    stacks (map staggers paddedCols)
  where
    -- Number of columns in the table
    numCols :: Int
    numCols = maximum (map length rss)

    -- Pad table so that every row has same number of columns
    square :: [[Rendered st]]
    square = map (padWith numCols empty) rss

    -- Transpose the table so we now have columns of rows of documents
    squareT :: [[Rendered st]]
    squareT = transpose square

    -- Pair every column with its desired width
    columnWidthsT :: [(Int, [Rendered st])]
    columnWidthsT = map (\rs -> (maximum $ map width rs, rs)) squareT

    -- Pad every cell in a single column to the width of that column
    paddedColsT :: [[Rendered st]]
    paddedColsT = map (\(w, rs) -> map (setWidth w) rs) columnWidthsT

    -- Transpose back
    paddedCols :: [[Rendered st]]
    paddedCols = transpose paddedColsT

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

padTo :: Int -> [Maybe (st, Char)] -> [Maybe (st, Char)]
padTo n = padWith n Nothing

padWith :: Int -> a -> [a] -> [a]
padWith n x xs = xs ++ replicate (n - length xs) x

-- | Remove padding
rTrim :: [Maybe (st, Char)] -> [Maybe (st, Char)]
rTrim = reverse . dropWhile isNothing . reverse
