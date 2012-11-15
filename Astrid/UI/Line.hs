module Astrid.UI.Line where
-- astrid:
import Astrid.Space.Line
import Astrid.Objects
-- curslet:
import UI.Curslet

lineKey :: Char -> Maybe D1
lineKey c = lookup c [('h', Back), ('l', Ahead)]

renderLine :: Curslet m w => Line Tile -> m ()
renderLine line = do
  let (Line b d a) = fmap seeTile line
  -- Center the cursor.
  (y, x) <- center
  -- Find out how much room we have on the right.
  (_, m) <- getmax
  let length = fromIntegral $ m - x
  -- Put the center and the things to the right.
  put . (d :) . take length $ a
  -- Move to the center-left.
  move (y, 0)
  -- Figure out what's on the left side.
  put . reverse . take (fromIntegral x) $ b
