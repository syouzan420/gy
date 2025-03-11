{-# LANGUAGE Arrows #-}
module EX1.MySF(mainSF) where

import FRP.Yampa (arr,filterE,integral,returnA,tag,edge,switch,mergeEvents
                 ,(>>^),(^<<),(>>>),SF,Event(..))
import Connector(toPInput,Input,PInput(..))
import Data.Point2 (point2X,point2Y)
import Data.AffineSpace((.+^))
import Data.Vector2 (vector2,vector2X,vector2Y)
import EX1.MyGL(clearAndRender)
import EX1.MyData(Obj(..),initBall)

brollBall :: Obj -> SF () Obj
brollBall ob = switch (bounce ob) brollBall

rollingBall :: Obj -> SF () Obj 
rollingBall (Obj p0 v0) = proc _ -> do
  p <- (p0 .+^) ^<< integral -< v0
  returnA -< Obj p v0 

bounce :: Obj -> SF () (Obj,Event Obj) 
bounce ob = proc _ -> do
  Obj p v <- rollingBall ob -< ()
  ev <- edge -< point2X p > 15 || point2X p < (-10)
  ev2 <- edge -< point2Y p < (-10) || point2Y p > 15 
  let tEv = ev `tag` Obj p (vector2 (vector2X v*(-1)) (vector2Y v))
      tEv2 = ev2 `tag` Obj p (vector2 (vector2X v) (vector2Y v*(-1))) 
      mEv = mergeEvents [tEv,tEv2]
  returnA -< (Obj p v, mEv)

perseInput :: SF (Event Input) (Event PInput)
perseInput = arr (fmap toPInput) 
              >>^ filterE (\k -> k==KeyH || k==KeyJ || k==KeyK || k==KeyL)

update :: SF (Event PInput) Obj 
update = proc _ -> do
  ball <- brollBall initBall -< () 
  returnA -< ball 

draw :: SF Obj (IO ())
draw = arr clearAndRender

mainSF :: SF (Event Input) (IO ())
mainSF = perseInput >>> update >>> draw 

