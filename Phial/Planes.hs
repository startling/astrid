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

-- | Encode possible directions to move into a type.
data Direction = N | E | S | W | NE | NW | SE | SW
  deriving (Eq, Ord, Show, Enum)

-- | Move the focus of a Plane in some direction.
pan :: Direction -> Simple Lens (Plane a) (Plane a)
pan d = case d of
  E -> iso (slices %~ shiftL) (view (pan W))
  S -> iso (slices %~ fmap shiftR) (view (pan N))
  W -> iso (slices %~ shiftR) (view (pan E))
  N -> iso (slices %~ fmap shiftL) (view (pan S))
  NE -> pan N . pan E; NW -> pan N . pan W
  SE -> pan S . pan E; SW -> pan S . pan W
  
-- | A lens on the focused tile of a plane.
focus :: Simple Lens (Plane a) a
focus = slices . during . during

-- | A lens on the tile in some direction.
adjacent :: Direction -> Simple Lens (Plane a) a
adjacent d = pan d . focus

-- | All the tiles surrounding the focus of the plane.
surround :: Plane d -> [d]
surround p = map (flip view p. adjacent) [N ..]
