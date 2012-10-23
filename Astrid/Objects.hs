module Astrid.Objects where
-- lens:
import Control.Lens

-- | A monster is something that knows how to move.
data Monster = Heroine
  deriving (Eq, Show, Enum)

-- | A tile is just a list of monsters.
type Tile = [Monster]

-- | Render a monster.
seeM :: Monster -> Char
seeM Heroine = '@'

-- | Render a tile.
seeTile :: Tile -> Char
seeTile [] = '.'
seeTile (a:_) = seeM a
