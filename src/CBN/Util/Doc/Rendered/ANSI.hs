module CBN.Util.Doc.Rendered.ANSI (
    write
  ) where

import Control.Monad
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
    go :: IORef Style -> [[Maybe (Style, Char)]] -> IO ()
    go ref = sequence_ . intersperse (putChar '\n') . map (goLine ref)

    goLine :: IORef Style -> [Maybe (Style, Char)] -> IO ()
    goLine ref = mapM_ (goChar ref) . rTrim

    goChar :: IORef Style -> Maybe (Style, Char) -> IO ()
    goChar _   Nothing        = putChar ' '
    goChar ref (Just (st, c)) = do
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

    toAnsiColor :: Color -> ANSI.Color
    toAnsiColor Blue  = ANSI.Blue
    toAnsiColor Red   = ANSI.Red
    toAnsiColor Green = ANSI.Green
