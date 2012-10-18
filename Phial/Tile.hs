module Phial.Tile where
import Phial.Planes

-- | Things that know how to move themselves.
data Monster = Heroine
  deriving (Show)

-- | Render a monster.
seeM :: Monster -> Char
seeM Heroine = '@'

-- | Things that never move.
data Fixture = Wall
  deriving (Eq, Show)

-- | Render a fixture.
seeF :: Fixture -> Char
seeF Wall = '#'

-- | A space that has some monsters and fixtures.
data Tile = Tile
  { monsters :: [Monster]
  , fixtures :: [Fixture] }
  deriving (Show)

-- | Render a tile.
seeT (Tile [] []) = '.'
seeT (Tile (a:_) _) = seeM a
seeT (Tile [] (a:_)) = seeF a

-- | An empty tile.
empty = Tile [] []

-- | A tile with just a wall in it.
walled = Tile [] [Wall]

-- | Tell whether this tile blocks the tiles behind it.
blocks :: Tile -> Bool
blocks = elem Wall . fixtures

-- | Turn an infinite plane into a (hopefully finite) plane
-- of visible tiles.
view :: Plane Tile -> Plane Tile
view (Plane s) = Plane . fmap viewNS . viewEW $ s
  where
    viewNS = both (takeWhile (not . blocks))
    viewEW = both (takeWhile (not . (blocks . during)))
