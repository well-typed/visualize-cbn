module CBN.Util.Doc.Style (
    Style(..)
  , Color(..)
  , groupByStyle
  ) where

import           Data.Default
import           Data.List    (groupBy)

data Style = Style {
      styleForeground :: Maybe Color
    , styleBackground :: Maybe Color
    , styleBold       :: Bool
    , styleItalic     :: Bool
    }
  deriving (Eq)

data Color =
    Blue
  | Red
  | Green
  deriving (Eq)

instance Default Style where
  def = Style {
      styleForeground = Nothing
    , styleBackground = Nothing
    , styleBold       = False
    , styleItalic     = False
    }

-- Are two characters the same style?
--
-- We regard padding as having a different style from everything else;
-- although it doesn't really matter what style we use for padding, if
-- we don't do this then something like
--
-- > (style1, "foo") `padding` (style2, "bar")
--
-- will not be rendered correctly, since @style1@ would be considered
-- equal to @padding@ which would in turn be considered equal to @style2@.
sameStyle :: Maybe (Style, Char) -> Maybe (Style, Char) -> Bool
sameStyle Nothing        _               = False
sameStyle _              Nothing         = False
sameStyle (Just (st, _)) (Just (st', _)) = st == st'

groupByStyle :: [Maybe (Style, Char)] -> [(Style, String)]
groupByStyle = map aux . groupBy sameStyle
  where
    -- After grouping, find each group and its style
    aux :: [Maybe (Style, Char)] -> (Style, String)
    aux []                 = (def, "")
    aux (Nothing     : cs) = let (st, str) = aux cs in (st, ' ':str)
    aux (Just (st,c) : cs) = (st, c:map toChar cs)

    toChar :: Maybe (Style, Char) -> Char
    toChar Nothing      = ' '
    toChar (Just (_,c)) = c
