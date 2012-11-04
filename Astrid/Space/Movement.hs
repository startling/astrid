module Astrid.Space.Movement where
-- containers:
import Control.Arrow ((&&&))
import Data.List (partition)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (isJust, maybeToList)
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
choose :: World w d => Maybe d -> w Tile -> Monster -> Maybe d
choose d _ Heroine = d

-- | A type for our intermediate mapping of movements.
type Movements d = Map d [Monster]

-- | Give each thing a chance to move; put the movements
-- in a Movements for the next step.
make :: (Ord d, World w d)
  => Maybe d -> w Tile -> (Tile, Movements d)
make d w = over _2 (foldr add M.empty . map f)
  -- Separate it into (Tile, [(Monster, Maybe d)]
  . over _1 (map fst) . partition (isJust . snd)
  -- Turn this whole thing into [(Monster, Maybe d)]
  . map ((snd &&& uncurry (choose d)) . (,) w) $ view focus w
  where
    f (m, Just x) = (x, m)
    -- add a v to a Map k [v].
    add :: Ord k => (k, v) -> Map k [v] -> Map k [v]
    add (a, b) m = insert m a . (:) b . maybe [] id
      . flip M.lookup m $ a
    -- insert with arguments flipped for convenience.
    insert m k v = M.insert k v m

-- | Move all the monsters that are in the midst of moving.
resolve :: (Ord d, World w d) => w (Tile, Movements d) -> [Monster]
resolve w = (w ^. focus . _1) ++ toHere w
  where
    -- Find all the monsters aimed at this tile.
    toHere :: (Ord d, World w d) => w (Tile, Movements d) -> [Monster]
    toHere w = flip concatMap directions $ \a -> maybe [] id
      (M.lookup (inverse a) (w ^. one a . _2))

-- | Move each monster, given the player character's move.
move :: (Ord d, Comonad w, World w d) => Maybe d -> w Tile -> w Tile
move d w = w =>> make d =>> resolve
-- TODO: verify that all moves are allowed.
