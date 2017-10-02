{-# LANGUAGE OverloadedStrings #-}
module CBN.Util.Doc.Rendered.HTML () where

import Control.Monad (replicateM_)
import Data.Char (isSpace)
import Data.Default
import Data.Function (on)
import Data.List (intersperse, groupBy)
import Data.Monoid
import Text.Blaze.Html5 (Html, toHtml, (!))
import Text.Blaze (ToMarkup(..))
import qualified Text.Blaze.Html5             as H
import qualified Text.Blaze.Html5.Attributes  as A

import CBN.Util.Doc.Rendered
import CBN.Util.Doc.Style

instance ToMarkup (Rendered Style) where
  toMarkup = go . rendered
    where
      go :: [[Maybe (Style, Char)]] -> Html
      go = sequence_ . intersperse H.br . map goLine

      goLine :: [Maybe (Style, Char)] -> Html
      goLine =
          sequence_ . map (goGroup . aux) . groupBy sameStyle . rTrim
        where
          -- After grouping, find each group and its style
          aux :: [Maybe (Style, Char)] -> (Style, String)
          aux []                 = (def, "")
          aux (Nothing     : cs) = let (st, str) = aux cs in (st, ' ':str)
          aux (Just (st,c) : cs) = (st, c:map toChar cs)

          toChar :: Maybe (Style, Char) -> Char
          toChar Nothing      = ' '
          toChar (Just (_,c)) = c

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

      goGroup :: (Style, String) -> Html
      goGroup (st, str)
        | st == def = goString str
        | otherwise = H.span ! A.style (styleToCss st) $ goString str

      -- Make all spaces non-breaking so that layout works as expected
      goString :: String -> Html
      goString = sequence_ . map aux . groupBy ((==) `on` isSpace)
        where
          aux :: String -> Html
          aux []       = error "impossible (groupBy)"
          aux cs@(c:_) = if isSpace c
                           then replicateM_ (length cs) nbsp
                           else toHtml cs

      styleToCss :: Style -> H.AttributeValue
      styleToCss Style{..} = (mconcat . concat) [
          [ "font-weight: bold;"
          | styleBold
          ]
        , [ "font-style: italic;"
          | styleItalic
          ]
        , [ "color: " <> toCssColor c <> ";"
          | Just c <- [styleForeground]
          ]
        , [ "background-color: " <> toCssColor c <> ";"
          | Just c <- [styleBackground]
          ]
        ]

      toCssColor :: Color -> H.AttributeValue
      toCssColor Blue = "darkblue"
      toCssColor Red  = "darkred"

nbsp :: Html
nbsp = preEscapedToMarkup ("&nbsp;" :: String)
