{-# Language FlexibleContexts #-}
{-# Language MultiParamTypeClasses #-}
module Phial where
-- base:
import Data.Array
-- mtl:
import Control.Applicative
import Control.Monad.State
-- phial:
import Phial.Classes
import Phial.Objects

data Direction = H | J | K | L
  deriving (Eq, Show)

-- Tell whether a given space is free.
free :: Coordinates -> Stage a -> Bool
free c s = maybe False null (query c s)

-- Move some coordinates accordinging to the direction.
moveXY :: Coordinates -> Direction -> Coordinates
moveXY (x, y) d = case d of
  H -> (x - 1, y); J -> (x, y - 1);
  L -> (x + 1, y); K -> (x, y + 1);

-- Move the hero, given a Direction and the coordinates of the hero.
turn :: MonadState (Stage Object) m =>
   Coordinates -> Direction -> m Coordinates
turn hero d = do
  let new = moveXY hero d
  b <- gets (free new)
  if b then gets (switch Hero hero new) >> return new
    else return hero

-- Run over a list of Directions.
play :: MonadState (Stage Object) m =>
   Coordinates -> [Direction] -> m Coordinates
play c [] = return c
play c (d:ds) = turn c d >>= flip play ds
