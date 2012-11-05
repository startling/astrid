{-# Language MultiParamTypeClasses #-}
module Astrid.Space.Plane where
-- lens:
import Control.Lens
-- comonad:
import Control.Comonad
-- astrid:
import Astrid.Space
import Astrid.Space.Line

-- | A plane is just a line of lines.
newtype Plane a = Plane (Line (Line a))
  deriving (Eq, Show, Ord)

-- | Planes are isomorphic to their (Line (Line a))s.
plane :: Simple Iso (Plane a) (Line (Line a))
plane = isomorphic (\fn -> fmap Plane . fn . unPlane)
  (\fn -> fmap unPlane . fn . Plane)
  where unPlane (Plane a) = a

data D2 = N | E | W | S
  deriving (Eq, Show, Ord)

instance Direction D2 where
  directions = [N, E, W, S]
  inverse N = S
  inverse E = W
  inverse S = N
  inverse W = E

instance Functor Plane where
  fmap fn (Plane a) = Plane . fmap (fmap fn) $ a

instance Space Plane D2 where
  focus = plane . focus . focus
  shift d fn (Plane a) = fn . Plane $ case d of
    N -> a ^. shift Ahead
    S -> a ^. shift Back
    W -> fmap (^. shift Ahead) a
    E -> fmap (^. shift Back) a

instance Comonad Plane where
  extract = view focus
  duplicate (Plane s) = fmap Plane . Plane . roll . roll $ s
    where
     roll :: Line (Line a) -> Line (Line (Line a))
     roll l = Line (to Back l) l (to Ahead l)
       where to d = let f = fmap (^. shift d) in iterate f . f
