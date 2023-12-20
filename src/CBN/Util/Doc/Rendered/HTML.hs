{-# LANGUAGE OverloadedStrings #-}
module CBN.Util.Doc.Rendered.HTML () where

import Control.Monad (replicateM_)
import Data.Char (isSpace)
import Data.Default
import Data.Function (on)
import Data.List (intersperse, groupBy)
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
        mapM_ goGroup . groupByStyle . rTrim

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
      toCssColor Blue  = "darkblue"
      toCssColor Red   = "darkred"
      toCssColor Green = "lightgreen"

nbsp :: Html
nbsp = preEscapedToMarkup ("&nbsp;" :: String)
