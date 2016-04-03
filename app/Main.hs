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

import qualified Codec.Picture as P

import Data.Version

import qualified Diagrams.Prelude as D
import Diagrams.Backend.Rasterific
import Diagrams.Core.Compile

import Graphics.UI.WX
import Graphics.UI.WX.Draw
import Graphics.UI.WX.Types

import Graphics.UI.WXCore.Image
import Graphics.UI.WXCore.WxcClassesAL
import Graphics.UI.WXCore.WxcClassTypes
import Graphics.UI.WXCore.WxcTypes

import Reactive.Banana
import Reactive.Banana.WX

import Glyph.Render
import Glyph.Types

import Paths_glyph_editor

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
        let bStroke :: Behavior Stroke
            bStroke = pure $ Stroke
              { strokeHead = []
              , strokeBody = [ StrokeStep { strokeStepPoint = D.mkP2 0.1 0.1
                                          , strokeStepWidth = 1
                                          , strokeStepKind  = NormalStep
                                          }
                             , StrokeStep { strokeStepPoint = D.mkP2 0.25 0.25
                                          , strokeStepWidth = 1
                                          , strokeStepKind  = NormalStep
                                          }
                             , StrokeStep { strokeStepPoint = D.mkP2 0.0 0.5
                                          , strokeStepWidth = 1
                                          , strokeStepKind  = NormalStep
                                          }
                             , StrokeStep { strokeStepPoint = D.mkP2 0.3 0.3
                                          , strokeStepWidth = 1
                                          , strokeStepKind  = NormalStep
                                          }
                             ]
              , strokeTail = []
              , strokeStartCap = SharpCap
              , strokeEndCap = SharpCap
              }

        sink p [on paint :== paintStroke <$> bStroke]

  network <- compile networkDescription
  actuate network

paintStroke :: Stroke -> DC a -> Rect -> IO ()
paintStroke stroke dc rect = do
  image <- imageCreateSized $ Size 300 300
  withPixelBuffer image $ copyImageToPixelBuffer rendered
  drawImage dc image (rectTopLeft rect) []
  imageDelete image
  where
    diagram = renderStroke stroke
    rendered = renderDia Rasterific options diagram
    options = RasterificOptions $ D.dims $ D.V2 300 300

copyImageToPixelBuffer :: P.Image P.PixelRGBA8 -> PixelBuffer -> IO ()
copyImageToPixelBuffer img pixbuf = mapM_ copyRow [0..P.imageHeight img - 1]
  where
    copyRow y = mapM_ (`copyPixel` y) [0..P.imageWidth img - 1]
    copyPixel x y = pixelBufferSetPixel pixbuf (Point x y) $
      pixelToColor $ P.pixelAt img x y

pixelToColor :: P.PixelRGBA8 -> Color
pixelToColor p@(P.PixelRGBA8 r g b a) = rgb (px r) (px b) (px g)
  where
    px :: P.Pixel8 -> P.Pixel8
    px v = round $ 255 * ((1 - a') + (a' * cv v))

    cv :: P.Pixel8 -> Float
    cv v = fromIntegral v / 255

    a' = fromIntegral a / 255

