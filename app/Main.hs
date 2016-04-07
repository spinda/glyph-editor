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

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}

import Data.Maybe
import Data.Version

import Diagrams.Prelude (P2, (^&), dims2D)
import Diagrams.Backend.Cairo
import Diagrams.Backend.WX

import Graphics.UI.WX
import Graphics.UI.WXCore.WxcClassesAL (imageCreateSized)
import Graphics.UI.WXCore.WxcClassTypes (Image)

import Reactive.Banana
import Reactive.Banana.WX

import Glyph.Types

import Paths_glyph_editor

import Model
import WX

main :: IO ()
main = start $ do
  f <- frame [text := "Glyph Editor " ++ showVersion version]
  p <- panel f []
  l <- singleListBox f []

  i <- imageCreateSized $ sz 300 300

  addButton   <- button f [text := "Add"]
  delButton   <- button f [text := "Remove"]
  clearButton <- button f [text := "Clear"]

  set f [ fullRepaintOnResize := False
        , layout              := row 5 [ minsize (sz 300 300) (widget p)
                                       , column 5 [ row 5 [ widget addButton
                                                          , widget delButton
                                                          , widget clearButton
                                                          ]
                                                  , fill $ widget l
                                                  ]
                                       ]
        ]

  let networkDescription :: MomentIO ()
      networkDescription = mdo
        eModel <- accumE initModel $ unions
          [ addStrokePoint  <$> eAddPoint
          , delStrokePoint  <$  eDelPoint
          , dragStrokePoint <$> eDragPoint
          , ((\d -> updateSelection . querySelector d) <$> bDiagram) <@> eSelect
          , addStroke   <$ eAddStroke
          , delStroke   <$ eDelStroke
          , clearStroke <$ eClearStroke
          , updateSelection . StrokeSelector <$> eSelectStroke
          ]
        bModel <- stepper initModel eModel

        sink l [ items :==
                   strokeListLabels . modelStrokes <$> bModel
               , selection :==
                   fromMaybe (-1) . selectedStroke . modelSelection <$> bModel
               ]

        let bDiagram = buildModelDiagram <$> bModel

        eAddStroke    <- event0 addButton   command
        eDelStroke    <- event0 delButton   command
        eClearStroke  <- event0 clearButton command
        eSelectStroke <- eventSelection l

        eMouse <- event1 p mouse

        let eSelect = fmap pointToP2 $ filterMouse eMouse $ \case
              MouseLeftDown{} -> True
              _ -> False
        let eAddPoint = fmap pointToP2 $ filterMouse eMouse $ \case
              MouseLeftDown _ m -> m == justShift
              _ -> False
        let eDelPoint = fmap pointToP2 $ filterMouse eMouse $ \case
              MouseLeftDown _ m -> m == justControl
              _ -> False
        let eDragPoint = fmap pointToP2 $ filterMouse eMouse $ \case
              MouseLeftDrag {} -> True
              _ -> False

        paintB p $ paintPanel i <$> bDiagram
        --reactimate $ print <$> eModel

  network <- compile networkDescription
  actuate network

paintPanel :: Image () -> ModelDiagram Cairo -> DC a -> Rect -> IO ()
paintPanel image diagram dc rect = do
  renderDiagramToImage diagram (dims2D 300 300) white image
  drawImage dc image (Point (rectLeft rect) (rectTop rect)) []

pointToP2 :: Point -> P2 Double
pointToP2 (Point x y) = (fromIntegral x / 300) ^& (1 - fromIntegral y / 300)

strokeListLabels :: [Stroke] -> [String]
strokeListLabels strokes = map (("Stroke " ++) . show) [1..length strokes]

