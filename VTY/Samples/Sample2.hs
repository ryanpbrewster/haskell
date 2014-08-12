import Graphics.Vty.Widgets.All
import Graphics.Vty.LLInput
import Graphics.Vty
import System.Exit (exitSuccess)
import qualified Data.Text as T

main = do
    tw <- plainText $ T.pack "foo"

    tw `onKeyPressed` \_ key _ ->
        if key == KASCII 'q' then exitSuccess
                             else do tw `appendText` (T.pack "!")
                                     return True

    fg <- newFocusGroup
    addToFocusGroup fg tw

    c <- newCollection

    -- you can run show_tw to ensure that the collection, c, shows tw
    show_tw <- addToCollection c tw fg

    runUi c defaultContext
