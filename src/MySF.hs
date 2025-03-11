{-# LANGUAGE Arrows #-}
module MySF(mainSF) where

import FRP.Yampa (arr,filterE,integral,returnA
                 ,(>>^),(^<<),(>>>),(^+^),SF,Event(..))
import Connector(clearAndRender,toPInput,Input,PInput(..))
import MyData(Obj(..),initBall)

rollingBall :: Obj -> SF () Obj 
rollingBall (Obj p0 v0) = proc _ -> do
  p <- (p0 ^+^) ^<< integral -< v0
  returnA -< Obj p v0 

perseInput :: SF (Event Input) (Event PInput)
perseInput = arr (fmap toPInput) 
              >>^ filterE (\k -> k==KeyH || k==KeyJ || k==KeyK || k==KeyL)

update :: SF (Event PInput) Obj 
update = proc _ -> do
  ball <- rollingBall initBall -< () 
  returnA -< ball 

draw :: SF Obj (IO ())
draw = arr clearAndRender

mainSF :: SF (Event Input) (IO ())
mainSF = perseInput >>> update >>> draw 

