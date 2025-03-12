module EX3.MyData(Pos,Vel,Obj(..),initBall) where

import Data.Point2(Point2(..))
import Data.Vector2(vector2,Vector2)

type Pos = Point2 Double
type Vel = Vector2 Double
data Obj = Obj !Pos !Vel

initBall :: Obj
initBall = Obj (Point2 (-10) 10) (vector2 15 (-5))

