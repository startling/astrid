module Phial.Tile where

-- | Things that know how to move themselves.
data Monster = Heroine

-- | Things that never move.
data Fixture = Wall

-- | A space that has some monsters and fixtures.
data Tile = Tile
  { monsters :: [Monster]
  , fixtures :: [Fixture] }
  deriving (Eq, Show)