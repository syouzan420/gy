module AppMain(appMain) where

import FRP.Yampa(reactInit,Event(..))
import Data.IORef(newIORef,writeIORef)
import Connector(initDrive,mainDrive,mainLoop,getTime)
import MySF(mainSF)

appMain :: IO ()
appMain = do
  newInput <- newIORef NoEvent
  oldTime <- newIORef (0 :: Int)
  h <- reactInit (initDrive >> return NoEvent)
                 (\_ _ b -> b >> return False)
                 mainSF 
  mainDrive newInput oldTime h
  oldTime' <- getTime
  writeIORef oldTime oldTime'
  mainLoop

