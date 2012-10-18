{-# Language DeriveFunctor #-}
module Phial.Classes where
-- base:
import Control.Applicative
import Data.Array
import Data.List (delete)
-- mtl:
import Control.Monad.State

-- Type synonym for coordinates.
type Coordinates = (Integer, Integer)

-- You can move inside some data structures.
class Container c where
  query :: Coordinates -> c a -> Maybe [a]
  -- remove some object from these coordinates.
  remove :: Eq a => a -> Coordinates -> c a -> Maybe (c a)
  -- put some object at these coordinates
  place :: a -> Coordinates -> c a -> Maybe (c a)

-- move a thing from one place to another in a Container.
switch :: (Eq a, Container c) =>
   a -> Coordinates -> Coordinates -> c a -> Maybe (c a)
switch a b c m = remove a b m >>= place a c

-- Some things know how to move on their own.
class Mover m where
  move :: Container c => m -> c m -> c m

-- Modify the state monad if some function returns Just.
maybeMod :: MonadState s m => (s -> Maybe s) -> m Bool
maybeMod f = gets f >>= maybe (return False)
  (\x -> put x >> return True)

-- Move a thing in the state monad.
moveIn :: (MonadState (c a) m, Mover a, Container c) => a -> m ()
moveIn a = modify (move a)

newtype Stage a = Stage
  { toArray :: Array Coordinates [a] }
  deriving (Functor, Eq, Show)

-- Stages are one kind of container.
instance Container Stage where
  query c a = if (snd . bounds . toArray $ a) >= c then 
    Just $ toArray a ! c else Nothing
  place x c a = query c a <&> \ms -> Stage $ toArray a // [(c, x : ms)] 
    where (<&>) = flip fmap
  remove x c a = query c a >>= \ms -> if x `elem` ms then
    Just . Stage $ toArray a // [(c, x `delete` ms)] else Nothing
