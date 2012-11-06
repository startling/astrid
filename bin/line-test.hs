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
main = runNcurses $ do
  -- Make a world empty but for our Heroine.
  let line = focus %~ (:) Heroine $ only empty
  refresh $ renderLine line
  getch >> return ()