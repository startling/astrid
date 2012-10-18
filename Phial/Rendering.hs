module Phial.Rendering where
-- base:
import Control.Applicative
-- curslet:
import UI.Curslet
-- phial:
import Phial.Planes
import Phial.Tile

-- | Move the cursor up.
up :: Curslet m w => m ()
up = do
  (y, x) <- position
  move (y - 1, x)

-- | Move the cursor down.
down :: Curslet m w => m ()
down = do
  (y, x) <- position
  move (y + 1, x)

-- | Move the cursor left.
left :: Curslet m w => m ()
left = do
  (y, x) <- position
  move (y, x - 1)

-- | Move the cursor right.
right :: Curslet m w => m ()
right = do
  (y, x) <- position
  move (y, x + 1)

-- | Add a character in place.
inPlace :: Curslet m w => Char -> m ()
inPlace c = addch c >> left

-- | Center along the y axis.
centerY :: Curslet m w => m (Integer, Integer)
centerY = do
  (y, _) <- getmax
  (_, x) <- position
  let c = (y `div` 2, x)
  move c >> return c

-- | Center along the x axis.
centerX :: Curslet m w => m (Integer, Integer)
centerX = do
  (_, x) <- getmax
  (y, _) <- position
  let c = (y, x `div` 2)
  move c >> return c

-- | Center the cursor on the screen
center :: Curslet m w => m (Integer, Integer)
center = centerX >> centerY >> position

-- | Render a (hopefully finite) plane in the Curslet monad.
renderPlane :: Curslet m w => Plane Char -> m ()
renderPlane (Plane (Line bs d as)) = do
  x <- fromIntegral . snd <$> center
  renderLine d
  flip mapM (take x bs) $ \l -> do
    left
    renderLine l
  m <- center >> fromIntegral . snd <$> getmax
  flip mapM (take (m - x) as) $ \l -> do
    right
    renderLine l
  return ()

-- | Render a (hopefully finite) line in the Curslet monad.
renderLine :: Curslet m w => Line Char -> m ()
renderLine (Line bs d as) = do
  return ()
  y <- fromIntegral . fst <$> centerY
  inPlace d
  flip mapM (take y bs) $ \c -> do
    up
    inPlace c
  y <- fromIntegral . fst <$> centerY
  n <- fromIntegral . fst <$> getmax
  flip mapM (take (n - y) as) $ \c -> do
    down
    inPlace c
  return ()
