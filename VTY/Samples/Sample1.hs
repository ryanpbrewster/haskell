import Graphics.Vty.Widgets.All
import qualified Data.Text as T

main = do
    e <- editWidget
    ui <- centered e
    fg <- newFocusGroup
    addToFocusGroup fg e
    c <- newCollection
    addToCollection c ui fg

    e `onActivate` \this ->
        getEditText this >>= (error . ("You entered: " ++) . T.unpack)

    runUi c defaultContext
