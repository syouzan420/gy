module EX0.MyGL(clearAndRender) where

import Graphics.UI.GLUT
import Data.Vector3(vector3,vector3X,vector3Y,vector3Z)
import Data.Vector2(vector2X,vector2Y)
import Data.List(uncons)
import EX0.MyData (Obj(..))

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
          where trVector = Vector3 (vc+vector3X p) (vc+vector3Y p) (vc+vector3Z p) 
        myRenderObject = (color red >>) . renderShapeAt (Sphere' 0.5 20 20)
        myRenderObjects obj = do 
          case uncons obj of
            Nothing -> return ()
            Just (Obj p _,xs) -> do
              myRenderObject $ vector3 (vector2X p) (vector2Y p) (-30)  
              myRenderObjects xs
          
