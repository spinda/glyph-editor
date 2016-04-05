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

{-# LANGUAGE FlexibleContexts #-}

module Model (
    pointToP2
  , initStroke
  , addStrokePoint
  , delStrokePoint
  , dragStrokePoint
  , renderPanel
  , getClickedPoint
  ) where

import Data.List
import Data.Maybe

import Diagrams.Prelude

import qualified Graphics.UI.WX as WX

import Glyph.Render
import Glyph.Types

pointToP2 :: Double -> Double -> WX.Point -> P2 Double
pointToP2 maxX maxY (WX.Point x y) =
  (fromIntegral x / maxX) ^& (1 - fromIntegral y / maxY)

pointHandleRadius :: Double
pointHandleRadius = 0.015

initStroke :: Stroke
initStroke = Stroke
  { strokeHead     = []
  , strokeBody     = []
  , strokeTail     = []
  , strokeStartCap = SharpCap
  , strokeEndCap   = SharpCap
  }

addStrokePoint :: P2 Double -> Stroke -> Stroke
addStrokePoint pt stroke = stroke
  { strokeBody = strokeBody stroke ++ [mkStrokeStep pt]
  }

delStrokePoint :: Int -> Stroke -> Stroke
delStrokePoint n stroke = stroke
  { strokeBody = before ++ after }
  where
    (before, _ : after) = splitAt n $ strokeBody stroke

dragStrokePoint :: P2 Double -> Int -> Stroke -> Stroke
dragStrokePoint pt n stroke = stroke
  { strokeBody = before ++ it' : after
  }
  where
    it' = it { strokeStepPoint = pt }
    (before, it : after) = splitAt n body
    body = strokeBody stroke

mkStrokeStep :: P2 Double -> StrokeStep
mkStrokeStep pt = StrokeStep
  { strokeStepPoint = pt
  , strokeStepWidth = 1
  , strokeStepKind  = NormalStep
  }

splitSelected :: Stroke -> Maybe Int -> (Maybe (P2 Double), [P2 Double])
splitSelected stroke Nothing =
  (Nothing, strokePoints stroke)
splitSelected stroke (Just n) =
  (Just it, before ++ after)
  where
    (before, it : after) = splitAt n $ strokePoints stroke

renderPanel :: Renderable (Path V2 Double) b
            => Stroke -> Maybe Int -> QDiagram b V2 Double Any
renderPanel stroke sel = (rectEnvelope (0 ^& 0) (1 ^& 1) dia) # bg white
  where
    dia = mconcat $ renderStroke stroke : handles
    handles =
      maybeToList (renderPointHandle True <$> it) ++
      (renderPointHandle False <$> rest)
    (it, rest) = splitSelected stroke sel

renderPointHandle :: Renderable (Path V2 Double) b
                  => Bool -> P2 Double -> QDiagram b V2 Double Any
renderPointHandle selected pt =
  moveTo pt $ circle pointHandleRadius # lc green # lw width
  where
    width
      | selected  = 2
      | otherwise = 1

getClickedPoint :: Stroke -> P2 Double -> Maybe Int
getClickedPoint stroke pt =
  findIndex (pointHandleContains pt) $ strokePoints stroke

pointHandleContains :: P2 Double -> P2 Double -> Bool
pointHandleContains test hndl = distance test hndl <= pointHandleRadius

