module CBN.Util.Doc.Rendered.String (toString) where

import Data.Char (isSpace)
import Data.List (intercalate)

import CBN.Util.Doc.Rendered

toString :: Rendered () -> String
toString = intercalate "\n" . map (rTrim . ignoreStyle) . rendered
  where
    -- remove any padding from the right
    rTrim :: String -> String
    rTrim = reverse . dropWhile isSpace . reverse

    ignoreStyle :: [((), Char)] -> String
    ignoreStyle = map snd
