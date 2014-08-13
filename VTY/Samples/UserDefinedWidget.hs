-- UserDefinedWidgets.hs
{-
 - Exploring using my own widgets
 -}

import Graphics.Vty.Widgets.All
import Graphics.Vty
import qualified Data.Text as T
import Data.Array
import Data.List (intercalate)
import System.Exit (exitSuccess)


{----------------------------------------------------------------------}
{- The keystone to this entire endeavor:                              -}
{- This is how you display a multi-line string using your own widgets -}
{----------------------------------------------------------------------}
multilineString attr str = foldl1 (<->) $ map (string attr) (lines str)

{-------------------------}
{- New Widget definition -}
{-------------------------}

newGridWidget init_str = do
    newWidget init_str $ \w ->
        w { render_ = \this size ctx -> do
                str <- getState this
                return $ multilineString (getNormalAttr ctx) str
          }

main = do
    let (rr,cc) = (5,5)
    let init_str = unlines $ replicate rr $ replicate cc 'x'

    gw <- newGridWidget init_str

    bgw <- boxFixed cc rr gw

    fg <- newFocusGroup
    fg `addToFocusGroup` bgw

    fg `onKeyPressed` \_ key _ ->
        case key of
            KEsc -> exitSuccess
            _ -> return False

    c <- newCollection
    showfg <- addToCollection c bgw fg

    runUi c defaultContext
