-- Copyright (c) 2016 Michael Smith.

-- This file is part of Glyph Editor.

-- Glyph Editor is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- Glyph Editor is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with Glyph Editor. If not, see <http://www.gnu.org/licenses/>.

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

