-- DrawingPictures.hs


import Graphics.UI.GLUT
import Control.Monad
 
my_points :: [(GLfloat,GLfloat,GLfloat)]
my_points = [ (sin (2*pi*k/12), cos (2*pi*k/12), 0) | k <- [1..12] ]
 
main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  _ <- createWindow ""
  displayCallback $= display
  mainLoop

display :: DisplayCallback
display = do 
  clear [ColorBuffer]
  renderPrimitive LineLoop $
     mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) my_points
  forM_ my_points $ \p -> renderCircle 0.1 p
  flush

renderCircle r (x,y,z) = do
    let circle_points = [ (x + r * cos t, y + r * sin t, z) | t <- [0, 0.2 .. 6.28] ]
    renderPrimitive Polygon $
        mapM_ (\(x,y,z) -> vertex $ Vertex3 x y z) circle_points
