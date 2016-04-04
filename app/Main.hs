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

import Control.Monad

import Data.Version

import qualified Diagrams.Prelude as D
import Diagrams.Backend.Rasterific
import Diagrams.Core.Compile

import Graphics.UI.WX as WX
import Graphics.UI.WX.Draw
import Graphics.UI.WX.Types

import Graphics.UI.WXCore.Image
import Graphics.UI.WXCore.WxcClassesAL
import Graphics.UI.WXCore.WxcClassTypes
import Graphics.UI.WXCore.WxcTypes

import Reactive.Banana as B
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
        eAddPoint <- event1' p click
        ePoints <- accumE [] $ unions
          [ (\p -> (++ [p])) <$> eAddPoint
          ]
        bPoints <- stepper [] ePoints

        let bStrokeSteps :: Behavior [StrokeStep]
            bStrokeSteps = map mkStrokeStep <$> bPoints

        let bStroke :: Behavior Stroke
            bStroke = mkStroke <$> bStrokeSteps

        sink p [on paint :== paintPanel <$> bStroke <*> bPoints]
        reactimate $ repaint p <$ eAddPoint
        reactimate $ print <$> eAddPoint

  network <- compile networkDescription
  actuate network

event1' :: w -> WX.Event w (a -> IO ()) -> MomentIO (B.Event a)
event1' widget e = do
  addHandler <- liftIO $ event1ToAddHandler' widget e
  fromAddHandler addHandler

event1ToAddHandler' :: w -> WX.Event w (a -> IO ()) -> IO (AddHandler a)
event1ToAddHandler' widget e = do
    (addHandler, runHandlers) <- newAddHandler
    set widget [on e := runHandlers]
    return addHandler

mkStrokeStep :: WX.Point -> StrokeStep
mkStrokeStep (Point x y) =
  StrokeStep (D.mkP2 (fromIntegral x / 300) (1 - fromIntegral y / 300)) 1 NormalStep

mkStroke :: [StrokeStep] -> Stroke
mkStroke strokes = Stroke
  { strokeHead = []
  , strokeBody = strokes
  , strokeTail = []
  , strokeStartCap = SharpCap
  , strokeEndCap = SharpCap
  }

paintPanel :: Stroke -> [WX.Point] -> DC a -> Rect -> IO ()
paintPanel stroke points dc rect = do
  image <- imageCreateSized $ Size 300 300
  withPixelBuffer image $ copyImageToPixelBuffer rendered
  drawImage dc image (rectTopLeft rect) []
  imageDelete image
  unless (null points) $ do
    mapM_ (drawPointHandle dc 1) (init points)
    drawPointHandle dc 2 $ last points
  where
    diagram = D.rectEnvelope (D.mkP2 0 0) (D.V2 1 1) $ renderStroke stroke
    rendered = renderDia Rasterific options diagram
    options = RasterificOptions $ D.dims $ D.V2 300 300

drawPointHandle :: DC a -> Int -> Point -> IO ()
drawPointHandle dc width (Point x y) =
  circle dc (Point (x - 2) (y - 2)) 4 [color := green, penWidth := width]

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

