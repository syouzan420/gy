{-# LANGUAGE Arrows #-}
module EX3.MySF(mainSF) where

import FRP.Yampa (arr,filterE,integral,returnA,tag,edge,switch,mergeEvents
                 ,(>>^),(^<<),(>>>),(^+^),SF,Event(..))
import Connector(toPInput,Input,PInput(..))
import Data.Point2 (point2X,point2Y)
import Data.AffineSpace((.+^))
import Data.Vector2 (vector2,vector2X,vector2Y)
import EX3.MyGL(clearAndRender)
import EX3.MyData(Vel,Obj(..),initBall)

brollBall :: Obj -> SF (Event PInput) Obj
brollBall ob = switch (bounce ob) brollBall

rollingBall :: Obj -> SF (Event PInput) Obj 
rollingBall (Obj p0 v0) = proc pin -> do
  v1 <- arr setVel -< (pin,vector2 0 0)
  v <- (v0 ^+^) ^<< integral -< v1
  p <- (p0 .+^) ^<< integral -< v
  returnA -< Obj p v 

bounce :: Obj -> SF (Event PInput) (Obj,Event Obj) 
bounce ob = proc pin -> do
  Obj p v <- rollingBall ob -< pin 
  ev <- edge -< point2X p > 15 || point2X p < (-10)
  ev2 <- edge -< point2Y p < (-10) || point2Y p > 15 
  let tEv = ev `tag` Obj p (vector2 (vector2X v*(-1)) (vector2Y v))
      tEv2 = ev2 `tag` Obj p (vector2 (vector2X v) (vector2Y v*(-1))) 
      mEv = mergeEvents [tEv,tEv2]
  returnA -< (Obj p v, mEv)

setVel :: (Event PInput,Vel) -> Vel
setVel (ei,v) = case ei of
  Event KeyL -> v ^+^ vector2 10 0
  Event KeyH -> v ^+^ vector2 (-10) 0
  Event KeyJ -> v ^+^ vector2 0 (-10)
  Event KeyK -> v ^+^ vector2 0 10
  _other -> v

perseInput :: SF (Event Input) (Event PInput)
perseInput = arr (fmap toPInput) 
              >>^ filterE (\k -> k==KeyH || k==KeyJ || k==KeyK || k==KeyL)

update :: SF (Event PInput) Obj 
update = proc pin -> do
  ball <- brollBall initBall -< pin 
  returnA -< ball 

draw :: SF Obj (IO ())
draw = arr clearAndRender

mainSF :: SF (Event Input) (IO ())
mainSF = perseInput >>> update >>> draw 

