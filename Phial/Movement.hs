module Phial.Movement where
import Control.Lens
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
-- TODO: actually query for Moves.
move :: Move -> Plane Tile -> (Tile, [(Monster, Move)])
move m (Plane p) = (p ^. during.during, [])

-- | Resolve the movements into this tile.
resolve :: Plane (Tile, [(Monster, Move)]) -> Tile
resolve (Plane p) = p ^. during.during._1
