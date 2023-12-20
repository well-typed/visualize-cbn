module CBN.Util.Doc.Rendered.String (toString) where

import Data.List (intercalate)

import CBN.Util.Doc.Rendered

toString :: Rendered style -> String
toString = intercalate "\n" . map (map (maybe ' ' snd)) . rendered
