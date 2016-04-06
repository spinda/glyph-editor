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
{-# LANGUAGE TupleSections #-}

module Model (
    -- * Editor Model Type
    Model(..)
  , Selector(..)
  , StrokeIndex
  , PointIndex
  , initModel
  , selectedStroke
  , selectedPoint

    -- * Model Diagram
  , ModelDiagram
  , buildModelDiagram
  , querySelector

    -- * Model Transformations
    -- ** Selection
  , updateSelection
    -- ** Stroke Manipulation
  , addStroke
  , delStroke
  , clearStroke
    -- ** Point Manipulation
  , addStrokePoint
  , delStrokePoint
  , dragStrokePoint
  ) where

import Data.Bifunctor
import Data.List
import Data.Maybe

import Diagrams.Prelude

import Glyph.Render
import Glyph.Types

--------------------------------------------------------------------------------
-- Editor Model Type -----------------------------------------------------------
--------------------------------------------------------------------------------

data Model = Model
  { modelStrokes   :: ![Stroke]
  , modelSelection :: !Selector
  } deriving (Show)

type StrokeIndex = Int
type PointIndex  = Int

data Selector = NullSelector
              | StrokeSelector !StrokeIndex
              | PointSelector  !StrokeIndex !PointIndex
                deriving (Eq, Show)

instance Ord Selector where
  compare x y | x == y = EQ

  compare NullSelector _ = LT
  compare _ NullSelector = GT

  compare StrokeSelector{} PointSelector{} = LT
  compare PointSelector{} StrokeSelector{} = GT

  compare (StrokeSelector x) (StrokeSelector y) = compare x y

  compare (PointSelector xs xp) (PointSelector ys yp)
    | xs == ys  = compare xp yp
    | otherwise = compare xs ys

instance Semigroup Selector where
  (<>) = max

instance Monoid Selector where
  mempty = NullSelector
  mappend = (<>)

initModel :: Model
initModel = Model
  { modelStrokes   = [emptyStroke]
  , modelSelection = StrokeSelector 0
  }

selectedStroke :: Selector -> Maybe StrokeIndex
selectedStroke NullSelector =
  Nothing
selectedStroke (StrokeSelector sn) =
  Just sn
selectedStroke (PointSelector sn _) =
  Just sn

selectedPoint :: Selector -> Maybe StrokeIndex
selectedPoint (PointSelector _ pn) =
  Just pn
selectedPoint _ =
  Nothing

--------------------------------------------------------------------------------
-- Model Diagram ---------------------------------------------------------------
--------------------------------------------------------------------------------

type ModelDiagram b = QDiagram b V2 Double Selector

buildModelDiagram :: Renderable (Path V2 Double) b => Model -> ModelDiagram b
buildModelDiagram (Model strokes sel) =
  (rectEnvelope (0 ^& 0) (1 ^& 1) dia) # bg' white
  where
    dia = mconcat $ reverse $ map (uncurry ofStroke) $ zip [0..] strokes
    bg' c d = d <> (boundingRect d # lwO 0 # fc c # value NullSelector)

    ofStroke sn stroke
      | selectedStroke sel == Just sn =
        mconcat $ (rendered # lc blue # fc blue) :
          map (uncurry (ofPoint sn)) (zip [0..] $ strokePoints stroke)
      | otherwise =
        rendered # lc black # fc black
      where
        rendered = renderStroke stroke # value (StrokeSelector sn)
    ofPoint sn pn pt
      | selectedPoint sel == Just pn =
        rendered # lw 2
      | otherwise =
        rendered # lw 1
      where
        rendered =
          (moveTo pt $ circle 0.015) # lc green # value (PointSelector sn pn)

querySelector :: ModelDiagram b -> P2 Double -> Selector
querySelector = sample

--------------------------------------------------------------------------------
-- Model Transformations -------------------------------------------------------
--------------------------------------------------------------------------------

-- Selection -------------------------------------------------------------------

updateSelection :: Selector -> Model -> Model
updateSelection NullSelector model = case modelSelection model of
  PointSelector sn _ -> model { modelSelection = StrokeSelector sn }
  _ -> model
updateSelection sel model = model { modelSelection = sel }

-- Stroke Manipulation ---------------------------------------------------------

addStroke :: Model -> Model
addStroke model@(Model strokes sel) = model
  { modelStrokes = addAfter emptyStroke sn strokes
  , modelSelection = StrokeSelector (sn + 1)
  }
  where
    sn = fromMaybe (length strokes - 1) $ selectedStroke sel

delStroke :: Model -> Model
delStroke model@(Model strokes sel) = case selectedStroke sel of
  Nothing -> model
  Just sn
    | length strokes == 1 -> initModel
    | otherwise -> model
      { modelStrokes = delAt sn strokes
      , modelSelection = StrokeSelector $ max 0 (sn - 1)
      }

clearStroke :: Model -> Model
clearStroke model@(Model strokes sel) = case selectedStroke sel of
  Nothing -> model
  Just sn -> model { modelStrokes = mapAt (const emptyStroke) sn strokes }

-- Point Manipulation ----------------------------------------------------------

addStrokePoint :: P2 Double -> Model -> Model
addStrokePoint pt model@(Model strokes sel) = case selectedStroke sel of
  Nothing -> model
  Just sn -> model
    { modelStrokes = mapAt (addStrokePoint' pt) sn strokes
    , modelSelection = PointSelector sn $ length $ strokeBody $ strokes !! sn
    }

addStrokePoint' :: P2 Double -> Stroke -> Stroke
addStrokePoint' pt stroke = stroke
  { strokeBody = strokeBody stroke ++ [normalStrokeStep pt 1]
  }


delStrokePoint :: Model -> Model
delStrokePoint model@(Model strokes sel) = case sel of
  PointSelector sn pn -> model
    { modelStrokes = mapAt (delStrokePoint' pn) sn strokes
    , modelSelection = case pn of
        0 | length (strokeBody $ strokes !! sn) == 1 -> StrokeSelector sn
          | otherwise -> PointSelector sn 0
        _ -> PointSelector sn (pn - 1)
    }
  _ -> model

delStrokePoint' :: PointIndex -> Stroke -> Stroke
delStrokePoint' pn stroke = stroke { strokeBody = delAt pn $ strokeBody stroke }


dragStrokePoint :: P2 Double -> Model -> Model
dragStrokePoint pt model@(Model strokes sel) = case sel of
  PointSelector sn pn -> model
    { modelStrokes = mapAt (dragStrokePoint' pt pn) sn strokes }
  _ -> model

dragStrokePoint' :: P2 Double -> PointIndex -> Stroke -> Stroke
dragStrokePoint' pt pn stroke = stroke
  { strokeBody = mapAt (mapStrokeStepPoint $ const pt) pn $ strokeBody stroke }

--------------------------------------------------------------------------------
-- Utility Functions -----------------------------------------------------------
--------------------------------------------------------------------------------

select :: Int -> [a] -> (a, [a])
select n xs = (it, before ++ after)
  where
    (before, it : after) = splitAt n xs

mapAt :: (a -> a) -> Int -> [a] -> [a]
mapAt f n xs = before ++ (f it : after)
  where
    (before, it : after) = splitAt n xs

delAt :: Int -> [a] -> [a]
delAt n = snd . select n

addAfter :: a -> Int -> [a] -> [a]
affAfter x (-1) xs = x : xs
addAfter x n xs = before ++ (it : x : after)
  where
    (before, it : after) = splitAt n xs

