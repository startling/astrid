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
move m (Plane p) = (,) tile $ flip map (monsters tile) . (id &&&)
  $ \c -> m
  where tile = p ^. during.during

-- | Resolve the movements into this tile.
resolve :: Plane (Tile, [(Monster, Move)]) -> Tile
resolve (Plane p) = p ^. during.during._1
