{-# Language MultiParamTypeClasses #-}
{-# Language FunctionalDependencies #-}
module Astrid.Space where
-- lens:
import Control.Lens

class Direction d where
  -- | A list of directions.
  directions :: [d]
  -- | Invert a given direction.
  inverse :: d -> d

-- | A space that we can move around in. Minimal complete definition
-- is 'focus' and either 'shift' or 'shifted'.
class Direction d => Space w d | w -> d where
  -- | Move the focus one cell in some direction.
  shift :: d -> w a -> w a
  -- | An isomorphism to this space shifted one unit in some direction.
  shifted :: (Functor f, Isomorphic k) => d
    -> k (w a -> f (w a)) (w a -> f (w a))
  shifted d = iso (shift d) (shift $ inverse d)
  -- | Traverse the origin tile of this space.
  focus :: Functor f => (a -> f a) -> w a -> f (w a)

-- | Traverse a neighboring tile of the origin in some space.
one :: (Space w d, Functor f) => d -> (a -> f a) -> w a -> f (w a)
one x = shifted x . focus
