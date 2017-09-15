module CBN.Util.Doc.Rendered.String (toString) where

import Data.List (intercalate)

import CBN.Util.Doc.Rendered

toString :: Rendered () -> String
toString = intercalate "\n" . map (ignoreStyle . rTrim) . rendered
  where
    ignoreStyle :: [Maybe ((), Char)] -> String
    ignoreStyle = map $ \mc -> case mc of
                                 Just ((), c) -> c
                                 Nothing      -> ' ' -- padding
