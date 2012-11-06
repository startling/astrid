{-# Language MultiParamTypeClasses #-}
module Astrid.Space.Line where
-- comonad:
import Control.Comonad (Comonad(..))
-- lens:
import Control.Lens ((^.), view)
-- astrid:
import Astrid.Space

-- | A line with tiles stretching in each direction to infinity.
data Line a = Line [a] a [a]
  deriving (Eq, Show, Ord)

-- | You can move in one of two directions along these lines.
data D1 = Ahead | Back
  deriving (Eq, Show, Enum)

instance Direction D1 where
  directions = [Ahead, Back]
  inverse Back = Ahead
  inverse Ahead = Back

instance Space Line D1 where
  focus fn (Line a b c) = fn b <&> \f -> Line a f c
    where (<&>) = flip fmap
  shift x fn (Line (a : b) c (d : e)) = fn $ case x of
    Ahead -> Line (c : a : b) d e
    Back -> Line b a (c : d : e)

instance Functor Line where
  fmap fn (Line b d a) = Line (map fn b) (fn d) (map fn a)

instance Comonad Line where
  extract = view focus
  extend f l = fmap f $ Line (to Back l) l (to Ahead l)
    where
     -- Make a list by repeatedly shifting this way.
     to d = let f = (^. shift d) in iterate f . f

-- | Utility function to limit a Line to x cells in each direction.
limit :: Int -> Line a -> Line a
limit n (Line a b c) = Line (take n a) b (take n c)

-- | Make a line composed entirely of this element.
only :: a -> Line a
only a = let r = repeat a in Line r a r