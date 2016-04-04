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

module WX (
    event1'
  , paintB
  , drawDiagram
  ) where

import Codec.Picture as P

import Diagrams.Prelude as D hiding (set)
import Diagrams.Backend.Rasterific
import Diagrams.Core.Compile

import Graphics.UI.WX as W
import Graphics.UI.WX.Draw
import Graphics.UI.WX.Types as W

import Graphics.UI.WXCore.Image
import Graphics.UI.WXCore.WxcClassesAL
import Graphics.UI.WXCore.WxcClassTypes
import Graphics.UI.WXCore.WxcTypes

import Reactive.Banana as R
import Reactive.Banana.Frameworks
import Reactive.Banana.WX

event1' :: w -> W.Event w (a -> IO ()) -> MomentIO (R.Event a)
event1' w e = do
  (addHandler, runHandlers) <- liftIO $ newAddHandler
  liftIO $ set w [on e := runHandlers]
  fromAddHandler addHandler

paintB :: Paint w => w -> Behavior (DC () -> Rect -> IO ()) -> MomentIO ()
paintB w b = do
  x <- valueBLater b
  liftIOLater $ set w [on paint := x]
  e <- changes b
  reactimate' $ (fmap $ \x -> set w [on paint := x] >> repaint w) <$> e

drawDiagram :: DC a -> W.Color -> QDiagram Rasterific V2 Double Any -> Rect -> IO ()
drawDiagram dc bgColor diagram rect = do
  image <- imageCreateSized $ Size (rectWidth rect) (rectHeight rect)
  withPixelBuffer image $ copyImageToPixelBuffer bgColor rendered
  drawImage dc image (rectTopLeft rect) []
  imageDelete image
  where
    rendered = renderDia Rasterific options diagram
    options = RasterificOptions $ dims size
    size = fromIntegral (rectWidth rect) ^& fromIntegral (rectHeight rect)

copyImageToPixelBuffer :: W.Color -> P.Image PixelRGBA8 -> PixelBuffer -> IO ()
copyImageToPixelBuffer bgColor img pixbuf = mapM_ copyRow [0..imageHeight img - 1]
  where
    copyRow y = mapM_ (`copyPixel` y) [0..imageWidth img - 1]
    copyPixel x y = pixelBufferSetPixel pixbuf (Point x y) $
      pixelToColor $ blendPixel bgColor' $ pixelAt img x y
    bgColor' = colorToPixel bgColor

blendPixel :: PixelRGB8 -> PixelRGBA8 -> PixelRGB8
blendPixel (PixelRGB8 r0 g0 b0) (PixelRGBA8 r g b a) =
  PixelRGB8 (px r0 r) (px g0 g) (px b0 b)
  where
    px :: Pixel8 -> Pixel8 -> Pixel8
    px x y = round $ 255 * (((1 - av) * cv x) + (av * cv y))

    cv :: Pixel8 -> Float
    cv v = fromIntegral v / 255

    av :: Float
    av = cv a

pixelToColor :: PixelRGB8 -> W.Color
pixelToColor (PixelRGB8 r g b) = rgb r g b

colorToPixel :: W.Color -> PixelRGB8
colorToPixel c = PixelRGB8 (colorRed c) (colorGreen c) (colorBlue c)

