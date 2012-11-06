-- astrid:
import Astrid.Objects
import Astrid.Space
import Astrid.Space.Line
import Astrid.UI.Line
-- curslet:
import UI.Curslet
import UI.Curslet.Ncurses
-- lens:
import Control.Lens

main :: IO ()
main = runNcurses $ run line
  -- Make a world empty but for our heroine.
  where line = focus %~ (:) Heroine $ only empty

run :: Curslet m w => Line Tile -> m ()
run world = do
  refresh $ renderLine world
  char <- getch
  case char of
    Just 'q' -> return ()
    Nothing -> run world
    Just c -> run world
