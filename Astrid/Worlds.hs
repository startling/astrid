{-# Language MultiParamTypeClasses #-}
{-# Language FunctionalDependencies #-}
module Astrid.Worlds where
-- lens:
import Control.Lens

class Direction d where
  -- | A list of directions.
  directions :: [d]
  -- | Invert a given direction.
  inverse :: d -> d

class Direction d => World w d | w -> d where
  -- | A lens on the worlds shifted from this one.
  shift :: d -> Simple Lens (w a) (w a)
  -- | The focused tile of this world.
  focus :: Simple Lens (w a) a

-- | A lens on a neighboring tile.
one :: (World w d) => d -> Simple Lens (w a) a
one x = shift x . focus
