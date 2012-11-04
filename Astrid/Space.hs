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
  -- | Traverse this space shifted one unit in any direction.
  shift :: Functor f => d -> (w a -> f (w a)) -> w a -> f (w a)
  shift = shifted
  -- | An isomorphism to this space shifted one unit in some direction.
  shifted :: (Functor f, Isomorphic k) => d
    -> k (w a -> f (w a)) (w a -> f (w a))
  shifted d = isomorphic (shift d) (shift $ inverse d)
  -- | Traverse the origin tile of this space.
  focus :: Functor f => (a -> f a) -> w a -> f (w a)

-- | Traverse a neighboring tile of the origin in some space.
one :: (Space w d, Functor f) => d -> (a -> f a) -> w a -> f (w a)
one x = shift x . focus
