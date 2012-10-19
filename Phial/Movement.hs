module Phial.Movement where
-- base:
import Control.Arrow ((&&&))
-- lens:
import Control.Lens
-- phial:
import Phial.Planes
import Phial.Tile

-- | Encode possible directions to move into a type.
data Direction = N | E | S | W | NE | NW | SE | SW
  deriving (Eq, Ord, Show, Enum)

-- | Things can move or stay still.
data Move = Null | Move Direction
  deriving (Eq, Show)

-- | Give all the monsters in a tile a chance to do stuff,
-- given the player character's move.
-- TODO : let non-pc monsters move.
move :: Move -> Plane Tile -> (Tile, [(Monster, Move)])
move m p = (,) tile . flip map (view monsters tile) $ (id &&& const m)
  where tile = p ^. focus

-- | Resolve the movements into this tile.
resolve :: Plane (Tile, [(Monster, Move)]) -> Tile
resolve p  = p ^.focus._1
