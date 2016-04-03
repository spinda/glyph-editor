{-# LANGUAGE RecursiveDo #-}

import Data.Version

import Graphics.UI.WX

import Reactive.Banana
import Reactive.Banana.WX

import Paths_glyph_editor

main :: IO ()
main = start $ do
  f <- frame [text := "Glyph Editor " ++ showVersion version]
  let l = label "Hello, World!"

  let networkDescription :: MomentIO ()
      networkDescription = mdo
        let bLayout :: Behavior Layout
            bLayout = pure $ minsize (sz 300 300) $ margin 10 $ column 5 $
              [floatCenter l]

        sink f [layout :== bLayout]

  network <- compile networkDescription
  actuate network

