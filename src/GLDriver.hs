module GLDriver(initDrive,mainDrive,mainLoop,getTime
               ,toPInput,Input,PInput(..)) where

import System.Exit(exitSuccess)
import Data.IORef(writeIORef,readIORef,IORef)
import FRP.Yampa(react,Event(..),ReactHandle)
import Graphics.UI.GLUT(($=),displayCallback,idleCallback,mainLoop,get,elapsedTime
                       ,keyboardMouseCallback,Key(..),KeyState(..),Modifiers)
import InitGL(initGL)

data Input = Keyboard !Key !KeyState !Modifiers

data PInput = KeyH | KeyJ | KeyK | KeyL | NoKey deriving Eq

initDrive :: IO ()
initDrive = initGL

mainDrive :: IORef (Event Input) -> IORef Int 
                            -> ReactHandle (Event Input) (IO ()) ->IO ()
mainDrive newInput oldTime rh = do
  displayCallback $= return ()
  keyboardMouseCallback $= Just
    (\k ks m _ -> if k==Char 'q'
                  then exitSuccess 
                  else writeIORef newInput (Event $ Keyboard k ks m))
  idleCallback $= Just (idle newInput oldTime rh)
  
getTime :: IO Int 
getTime = get elapsedTime

idle :: IORef (Event Input) -> IORef Int 
            -> ReactHandle (Event Input) (IO ()) -> IO ()
idle newInput oldTime rh = do
  newInput' <- readIORef newInput
  newTime' <- get elapsedTime
  oldTime' <- get oldTime
  let dt = fromIntegral (newTime' - oldTime')/1000
  _ <- react rh (dt, Just newInput')
  writeIORef oldTime newTime'
  return ()

toPInput :: Input -> PInput
toPInput (Keyboard (Char 'h') Down _) = KeyH
toPInput (Keyboard (Char 'j') Down _) = KeyJ 
toPInput (Keyboard (Char 'k') Down _) = KeyK 
toPInput (Keyboard (Char 'l') Down _) = KeyL 
toPInput _ = NoKey

