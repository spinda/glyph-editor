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
    paintB
  ) where

import Graphics.UI.WX as W

import Reactive.Banana as R
import Reactive.Banana.Frameworks

paintB :: Paint w => w -> Behavior (DC () -> Rect -> IO ()) -> MomentIO ()
paintB w b = do
  x <- valueBLater b
  liftIOLater $ set w [on paint := x]
  e <- changes b
  reactimate' $ (fmap $ \y -> set w [on paint := y] >> repaint w) <$> e

