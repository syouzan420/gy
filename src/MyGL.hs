module MyGL(initGL,clearAndRender) where

import Graphics.UI.GLUT
import Data.Vector3(vector3,vector3X,vector3Y,vector3Z)
import Data.Vector2(vector2X,vector2Y)
import Data.List(uncons)
import MyData (Obj(..))

type R = GLdouble

initGL :: IO ()
initGL = do
  _ <- getArgsAndInitialize
  _ <- createWindow "Bounce"
  initialDisplayMode $= [ WithDepthBuffer, DoubleBuffered ]
  windowSize         $= Size 480 480 
  depthFunc          $= Just Less
  clearColor         $= Color4 0 0 0 0
  light (Light 0)    $= Enabled
  lighting           $= Enabled
  lightModelAmbient  $= Color4 0.5 0.5 0.5 1
  diffuse (Light 0)  $= Color4 1 1 1 1
  blend              $= Enabled
  blendFunc          $= (SrcAlpha, OneMinusSrcAlpha)
  colorMaterial      $= Just (FrontAndBack, AmbientAndDiffuse)
  reshapeCallback    $= Just resizeScene
  return ()

resizeScene :: Size -> IO ()
resizeScene (Size w 0) = resizeScene (Size w 1) -- prevent divide by zero
resizeScene s@(Size width height) = do
  viewport    $= (Position 0 0, s)
  matrixMode  $= Projection
  loadIdentity
  perspective 45 (w2/h2) 1 1000
  matrixMode  $= Modelview 0
 where
    w2 = half width
    h2 = half height
    half z = realToFrac z / 2

clearAndRender :: Obj -> IO ()
clearAndRender ob = do
  clear [ ColorBuffer, DepthBuffer ]
  loadIdentity
  myRenderObjects [ob]
  flush
  where size2 :: R
        size2 = 6/2
        red    = Color4 1.0 0.7 0.8 1.0 :: Color4 R
        renderShapeAt s p = preservingMatrix $ do
          translate $ Vector3 (0.5 - size2 + vector3X p)
                              (0.5 - size2 + vector3Y p)
                              (0.5 - size2 + vector3Z p)
          renderObject Solid s                              
        myRenderObject = (color red >>) . renderShapeAt (Sphere' 0.5 20 20)
        myRenderObjects obj = do 
          case uncons obj of
            Nothing -> return ()
            Just (Obj p _,xs) -> do
              myRenderObject $ vector3 (vector2X p) (vector2Y p) (-30)  
              myRenderObjects xs
          
