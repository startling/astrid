module Astrid.Space.Movement where
-- containers:
import Control.Arrow ((&&&))
import Data.List (partition)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (isJust, maybeToList)
import Data.Foldable (foldMap)
-- comonad:
import Control.Comonad
-- lens:
import Control.Lens
-- astrid:
import Astrid.Space
import Astrid.Objects

-- | Given the player's chosen direction to move, let all
-- monsters choose their directions. The player needs to show
-- his choice so this can be a total function, not for any
-- nefarious cheaty reasons.
choose :: Space w d => Maybe d -> w Tile -> Monster -> Maybe d
choose d _ Heroine = d

-- | A type for our intermediate mapping of movements.
type Movements d = Map d [Monster]

-- | Give each thing a chance to move; put the movements
-- in a Movements for the next step.
make :: (Ord d, Space w d)
  => Maybe d -> w Tile -> (Tile, Movements d)
make d w = _2 %~ foldr add M.empty $ turn (choose d w) (view focus w)
  where
    -- Given an "a -> Maybe b" function, sort out the ones for whom
    -- the function returns Nothing from an [a].
    turn :: (a -> Maybe b) -> [a] -> ([a], [(a, b)])
    turn fn = foldMap $ \a -> maybe ([a], [])
      (\n -> ([], [(a, n)])) (fn a)
    -- Build a "Map d [m]" out of some (m, d) pairs.
    add :: Ord d => (m, d) -> Map d [m] -> Map d [m]
    add (m, a) = at a %~ Just . maybe [m] (m :)

-- | Move all the monsters that are in the midst of moving.
resolve :: (Ord d, Space w d) => w (Tile, Movements d) -> [Monster]
resolve w = (w ^. focus . _1) ++ toHere w
  where
    -- Find all the monsters aimed at this tile.
    toHere :: (Ord d, Space w d) => w (Tile, Movements d) -> [Monster]
    toHere w = flip concatMap directions $ \a -> maybe [] id
      (M.lookup (inverse a) (w ^. one a . _2))

-- | Move each monster, given the player character's move.
go :: (Ord d, Comonad w, Space w d) => Maybe d -> w Tile -> w Tile
go d w = w =>> make d =>> resolve
-- TODO: verify that all moves are allowed.
