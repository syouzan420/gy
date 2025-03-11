module InitGL(initGL) where

import Graphics.UI.GLUT

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

