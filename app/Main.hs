{-# LANGUAGE RecursiveDo #-}

import Data.Version

import Graphics.UI.WX
import Graphics.UI.WX.Draw
import Graphics.UI.WX.Types

import Reactive.Banana
import Reactive.Banana.WX

import Paths_glyph_editor

main :: IO ()
main = start $ do
  f <- frame [text := "Glyph Editor " ++ showVersion version]
  p <- panel f []

  let networkDescription :: MomentIO ()
      networkDescription = mdo
        let bText :: Behavior String
            bText = pure "Hello, World!"

        let bLayout :: Behavior Layout
            bLayout = pure $ minsize (sz 300 300) $ fill $ widget p

        sink p [on paint :== paintText <$> bText]
        sink f [layout :== bLayout]

  network <- compile networkDescription
  actuate network

paintText :: String -> DC a -> Rect -> IO ()
paintText txt dc rect = drawText dc txt (rectTopLeft rect) []

