module MyData(Pos,Vel,Obj(..),initBall) where

import Data.Vector2(vector2,Vector2)

type Pos = Vector2 Double
type Vel = Vector2 Double
data Obj = Obj !Pos !Vel

initBall :: Obj
initBall = Obj (vector2 (-10) 10) (vector2 4 0)

