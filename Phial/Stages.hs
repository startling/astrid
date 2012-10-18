module Phial.Stages where
import Phial.Planes
import Phial.Tile

-- | A plane with just a room in the focused area.
room :: Plane Tile
room = Plane $ Line side slice side
  where
    side :: [Line Tile]
    side = replicate 3 slice ++ repeat blocked
    floor :: [Tile]
    floor = replicate 3 empty ++ repeat walled
    -- A Line that's empty in the middle.
    slice :: Line Tile
    slice = Line floor empty floor
    -- A line that's completely filled with walls.
    blocked :: Line Tile
    blocked = Line (repeat walled) walled (repeat walled)
