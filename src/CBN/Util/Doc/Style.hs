module CBN.Util.Doc.Style (
    Style(..)
  , Color(..)
  ) where

import Data.Default

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
  deriving (Eq)

instance Default Style where
  def = Style {
      styleForeground = Nothing
    , styleBackground = Nothing
    , styleBold       = False
    , styleItalic     = False
    }
