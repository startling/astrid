module Phial.Planes where
{- idea adapted from
   http://blog.sigfpe.com/2006/12/evaluating-cellular-automata-is.html -}
import Control.Comonad

data Line a = Line
  { before :: [a]
  , during :: a
  , after  :: [a] }

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
newtype Plane a = Plane (Line (Line a))

instance Functor Plane where
  fmap fn (Plane l) = Plane . fmap (fmap fn) $ l

instance Comonad Plane where
  extract (Plane s) = extract . extract $ s
  duplicate (Plane s) = fmap Plane . Plane . roll . roll $ s
    where roll a = Line (tail . iterate (fmap shiftL) $ a) a
                        (tail . iterate (fmap shiftR) $ a)
