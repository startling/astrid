{-# Language TemplateHaskell #-}
module Phial.Planes where
{- idea adapted from
   http://blog.sigfpe.com/2006/12/evaluating-cellular-automata-is.html -}
-- comonad:
import Control.Comonad
-- lens:
import Control.Lens

data Line a = Line
  { _before :: [a]
  , _during :: a
  , _after  :: [a] }
makeLenses ''Line

both :: ([a] -> [a]) -> Line a -> Line a
both f (Line b d a) = Line (f b) d (f a)

instance Functor Line where
  fmap fn (Line a b c) = Line (map fn a) (fn b) (map fn c)

instance Comonad Line where
  extract (Line _ a _) = a
  duplicate a = Line (tail $ iterate shiftL a)
    a (tail $ iterate shiftR a)

-- | Move focus one tile leftwards.
shiftL :: Line a -> Line a
shiftL (Line (b:a) c d) = Line a b (c:d)

-- | Move focus one tile rightwards.
shiftR :: Line a -> Line a
shiftR (Line a b (c:d)) = Line (b:a) c d

-- | The two-dimensional equivalent of 'Line'.
newtype Plane a = Plane
  { _slices :: (Line (Line a)) }
makeLenses ''Plane

instance Functor Plane where
  fmap fn (Plane l) = Plane . fmap (fmap fn) $ l

instance Comonad Plane where
  extract (Plane s) = extract . extract $ s
  duplicate (Plane s) = fmap Plane . Plane . roll . roll $ s
    where roll a = Line (tail . iterate (fmap shiftL) $ a) a
                        (tail . iterate (fmap shiftR) $ a)

-- | Pan a plane leftwards.
panLeft :: Plane a -> Plane a
panLeft = slices %~ shiftL

-- | Pan a plaine rightwards.
panRight :: Plane a -> Plane a
panRight = slices %~ shiftR

-- | Pan a plane upwards.
panUp :: Plane a -> Plane a
panUp = slices %~ fmap shiftL

-- | Pan a plane downwards.
panDown :: Plane a -> Plane a
panDown = slices %~ fmap shiftR

-- | A lens on the focused tile of a plane.
focused :: Simple Lens (Plane a) a
focused = slices . during . during

-- | Get the tile to the immediate left.
toLeft = view focused . panLeft

-- | Get the tile to the immediate right.
toRight = view focused . panLeft

-- | Get the tile immediately south of the current one.
below :: Plane d -> d
below = view focused . panDown

-- | Get the tile immediately north of the current one.
above :: Plane d -> d
above = view focused . panUp
