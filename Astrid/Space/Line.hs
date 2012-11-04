{-# Language TemplateHaskell #-}
{-# Language MultiParamTypeClasses #-}
module Astrid.Space.Line where
-- comonad:
import Control.Comonad
-- lens:
import Control.Lens 
-- astrid:
import Astrid.Space

-- | A line with tiles stretching in each direction to infinity.
data Line a = Line
  { _before :: [a]
  , _during ::  a
  , _after  :: [a] }
  deriving (Eq, Show, Ord)
makeLenses ''Line

-- | You can move in one of two directions along these lines.
data D1 = Ahead | Back
  deriving (Eq, Show, Enum)

instance Direction D1 where
  directions = [Ahead, Back]
  inverse Back = Ahead
  inverse Ahead = Back

instance World Line D1 where
  focus = during
  shift x fn (Line (a : b) c (d : e)) = fn $ case x of
    Back -> Line (c : a : b) d e
    Ahead -> Line b a (c : d : e)

instance Functor Line where
  fmap fn (Line b d a) = Line (map fn b) (fn d) (map fn a)

instance Comonad Line where
  extract = view during
  extend f l = flip Line (f l)
    (map f . done (^. shift Back) $ l)
    (map f . done (^. shift Ahead) $ l)
    where done f x = iterate f (f x)
