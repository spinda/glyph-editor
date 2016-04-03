import Data.Version

import Graphics.UI.WX

import Paths_glyph_editor

main :: IO ()
main = start $ do
  f <- frame [text := "Glyph Editor " ++ showVersion version]
  let l = label "Hello, World!"
  set f [layout := minsize (sz 300 300) $ margin 10 $ column 5 [floatCenter l]]

