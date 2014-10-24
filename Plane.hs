module Plane where

import qualified Data.Vect.Double                   as V
-- import Debug.Trace

data Plane
 = Plane
 { _pOrg    :: !V.Vec2
 , _pVel    :: !V.Vec2
 , _pRot    :: !Double
 , _pRotVel :: !Double
 }

data PlaneControl
 = PlaneControl
 { _pcBoost  :: !Double
 , _pcFlaps  :: !Double
 }

plane :: Plane
plane
 = Plane
 { _pOrg    = V.zero
 , _pVel    = V.zero
 , _pRot    = 0
 , _pRotVel = 0
 }

planeControl :: PlaneControl
planeControl
 = PlaneControl
 { _pcBoost  = 0
 , _pcFlaps  = 0
 }

accOfControl :: PlaneControl -> Plane -> (V.Vec2, Double)
accOfControl pc p
 = let boost = V.sinCosRadius (_pRot p) (_pcBoost pc)
       v'    = _pVel    p V.&+ boost
       flap  = _pcFlaps pc * V.norm v'
   in (boost, flap)


fly :: PlaneControl -> Plane -> Double -> Plane
fly pc p time
 = let (b,f) = accOfControl pc p

       v'    = _pVel p V.&+ ((b V.&+ gravity) V.&* time)
       rv'   = _pRotVel p + (f * time)
       rv''  = rv' * (fudge_turningfriction ** time)
   in  update (p { _pVel = v', _pRotVel = rv'' }) time

update :: Plane -> Double -> Plane
update p time
 = let o     = _pOrg p
       v     = _pVel p V.&* time
       o'    = o V.&+ v
       r     = _pRot p
       r'    = r    + (_pRotVel p * time)

       -- Final thing: look at difference between rot and velocity
       go    = V.sinCos r

       ang   = V.angle v go
       ang'  | V.normsqr v == 0
             = 0
             | (V.rotateCW go V.&. v) < 0
             = -ang
             | otherwise
             = ang

       l     = V.norm  v
       rr'   = r' - (ang' * l * time * (fudge_turntowards ** time))


   in p { _pOrg = o'
        , _pRot = rr' }

gravity :: V.Vec2
gravity  = V.mkVec2 (0, -9.0)

fudge_turningfriction :: Double
fudge_turningfriction = 0.1

fudge_turntowards :: Double
fudge_turntowards = 0.001
