module CBN.Util.Doc.Rendered.ANSI (
    write
  ) where

import Control.Monad
import Data.Char (isSpace)
import Data.Default
import Data.IORef
import Data.List (intersperse)
import qualified System.Console.ANSI as ANSI

import CBN.Util.Doc.Rendered
import CBN.Util.Doc.Style

write :: Rendered Style -> IO ()
write r = do
    stRef <- newIORef def
    go stRef $ rendered r
    ANSI.setSGR [ANSI.Reset]
  where
    go :: IORef Style -> [[(Style, Char)]] -> IO ()
    go ref = sequence_ . intersperse (putChar '\n') . map (goLine ref)

    goLine :: IORef Style -> [(Style, Char)] -> IO ()
    goLine ref cs = do mapM_ (uncurry (goChar ref)) (rTrim cs)

    goChar :: IORef Style -> Style -> Char -> IO ()
    goChar ref st c = do
      activeStyle <- readIORef ref
      when (activeStyle /= st) $ do
        ANSI.setSGR (styleToSGR st)
        writeIORef ref st
      putChar c

    styleToSGR :: Style -> [ANSI.SGR]
    styleToSGR Style{..} = mconcat [
        [ ANSI.Reset ]
      , [ ANSI.SetConsoleIntensity ANSI.BoldIntensity
        | styleBold
        ]
      , [ ANSI.SetItalicized True
        | styleItalic
        ]
      , [ ANSI.SetColor ANSI.Foreground ANSI.Dull (toAnsiColor c)
        | Just c <- [styleForeground]
        ]
      , [ ANSI.SetColor ANSI.Background ANSI.Dull (toAnsiColor c)
        | Just c <- [styleBackground]
        ]
      ]

    -- Remove unnecessary padding
    rTrim :: [(Style, Char)] -> [(Style, Char)]
    rTrim = reverse . dropWhile (isSpace . snd) . reverse

    toAnsiColor :: Color -> ANSI.Color
    toAnsiColor Blue = ANSI.Blue
    toAnsiColor Red  = ANSI.Red
