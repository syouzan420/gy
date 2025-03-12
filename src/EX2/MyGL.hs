module EX2.MyGL(clearAndRender) where

import Graphics.UI.GLUT
import Data.Point2(point2X,point2Y)
import Data.Point3(Point3(..),point3X,point3Y,point3Z)
import Data.List(uncons)
import EX2.MyData (Obj(..))

type R = GLdouble

clearAndRender :: Obj -> IO ()
clearAndRender ob = do
  clear [ ColorBuffer, DepthBuffer ]
  loadIdentity
  myRenderObjects [ob]
  flush
  where size2 :: R
        size2 = 6/2
        vc = 0.5 - size2
        red    = Color4 1.0 0.7 0.8 1.0 :: Color4 R
        renderShapeAt s p = preservingMatrix $ do
          translate trVector
          renderObject Solid s                              
          where trVector = Vector3 (vc+point3X p) (vc+point3Y p) (vc+point3Z p) 
        myRenderObject = (color red >>) . renderShapeAt (Sphere' 0.5 20 20)
        myRenderObjects obj = do 
          case uncons obj of
            Nothing -> return ()
            Just (Obj p _,xs) -> do
              myRenderObject $ Point3 (point2X p) (point2Y p) (-30)  
              myRenderObjects xs
          
