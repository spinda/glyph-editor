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
  , filterMouse
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

filterMouse :: R.Event EventMouse -> (EventMouse -> Bool) -> R.Event W.Point
filterMouse e f = mousePos <$> filterE f e

paintB :: Paint w => w -> Behavior (DC () -> Rect -> IO ()) -> MomentIO ()
paintB w b = do
  x <- valueBLater b
  liftIOLater $ set w [on paint := x]
  e <- changes b
  reactimate' $ (fmap $ \x -> set w [on paint := x] >> repaint w) <$> e

drawDiagram :: (Monoid b, Semigroup b)
            => DC a -> QDiagram Rasterific V2 Double b -> Rect -> IO ()
drawDiagram dc diagram rect = drawJuicyImage dc rendered rect
  where
    rendered = renderDia Rasterific options diagram
    options = RasterificOptions $ dims size
    size = fromIntegral (rectWidth rect) ^& fromIntegral (rectHeight rect)

drawJuicyImage :: DC a -> P.Image PixelRGBA8 -> Rect -> IO ()
drawJuicyImage dc img rect = do
  img' <- imageCreateFromPixels size colors
  drawImage dc img' (rectTopLeft rect) []
  imageDelete img'
  where
    size = Size (rectWidth rect) (rectHeight rect)
    colors = imageToColors img

imageToColors :: P.Image PixelRGBA8 -> [W.Color]
imageToColors img = concatMap getRow [0..imageHeight img - 1]
  where
    getRow y = map (`getPixel` y) [0..imageWidth img - 1]
    getPixel x y = pixelToColor $ pixelAt img x y

pixelToColor :: PixelRGBA8 -> W.Color
pixelToColor (PixelRGBA8 r g b a) = rgba r g b a

