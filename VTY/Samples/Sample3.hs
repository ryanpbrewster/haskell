-- Sample3.hs
{-
 - Trying to get a number to increment on every button press
 -}

import Graphics.Vty.Widgets.All
import Graphics.Vty
import qualified Data.Text as T
import System.Exit (exitSuccess)

data Counter = Counter Int deriving (Show, Eq, Ord)

newCounterWidget = do
    newWidget (Counter 0) $ \w ->
        w { render_ = \this size ctx -> do
                Counter v <- getState this
                return $ string (getNormalAttr ctx) (show v)
          }

incCounterWidget wRef = updateWidgetState wRef incCounter

incCounter (Counter v) = Counter (v+1)

main = do
    cw <- newCounterWidget

    fg <- newFocusGroup
    fg `addToFocusGroup` cw

    fg `onKeyPressed` \_ key _ ->
        case key of
            KEsc -> exitSuccess
            _ -> do incCounterWidget cw
                    return True

    c <- newCollection
    addToCollection c cw fg

    runUi c defaultContext
