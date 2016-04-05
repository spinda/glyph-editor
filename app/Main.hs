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
{-# LANGUAGE TupleSections #-}

import Data.Maybe
import Data.Version

import Diagrams.Prelude (Any, P2, QDiagram, V2)
import Diagrams.Backend.Rasterific

import Graphics.UI.WX

import Reactive.Banana
import Reactive.Banana.Frameworks
import Reactive.Banana.WX

import Glyph.Render
import Glyph.Types

import Paths_glyph_editor

import Model
import WX

main :: IO ()
main = start $ do
  f <- frame [text := "Glyph Editor " ++ showVersion version]
  p <- panel f []
  set f [ layout              := fill $ widget p
        , outerSize           := sz 300 300
        , fullRepaintOnResize := False
        ]

  let networkDescription :: MomentIO ()
      networkDescription = mdo
        eClickPanel      <- ((processClick <$> bStroke) <@>) <$> event1' p click
        eRightClickPanel <- ((processClick <$> bStroke) <@>) <$> event1' p clickRight
        eDragPanel       <- fmap (pointToP2 300 300) <$> event1' p drag

        let eAddPoint   = fst <$> filterE (isNothing . snd) eClickPanel
        let eClickPoint = filterJust $ (\(p, n) -> (p, ) <$> n) <$> eClickPanel
        let eDragPoint  = filterJust $ ((\s pt -> (pt, ) <$> s) <$> bSelectedPoint) <@> eDragPanel
        let eDelPoint   = filterJust $ snd <$> eRightClickPanel

        eStroke <- accumE initStroke $ unions
          [ addStrokePoint <$> eAddPoint
          , uncurry dragStrokePoint <$> eDragPoint
          , delStrokePoint <$> eDelPoint
          ]
        bStroke <- stepper initStroke eStroke

        bSelectedPoint <- accumB Nothing $ unions
          [ (const . Just . snd) <$> eClickPoint
          , (const . Just . length . strokePoints <$> bStroke) <@ eAddPoint
          , (const Nothing) <$ eDelPoint
          ]

        paintB p $ paintPanel <$> bStroke <*> bSelectedPoint

  network <- compile networkDescription
  actuate network

processClick :: Stroke -> Point -> (P2 Double, Maybe Int)
processClick stroke pt = (pt', getClickedPoint stroke pt')
  where
    pt' = pointToP2 300 300 pt

paintPanel :: Stroke -> Maybe Int -> DC a -> Rect -> IO ()
paintPanel stroke sel dc rect =
  drawDiagram dc diagram (rect { rectWidth = 300, rectHeight = 300 })
  where
    diagram = renderPanel stroke sel

