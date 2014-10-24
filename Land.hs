module Land where
import qualified Data.Vect.Double    as V
import qualified Data.Vector.Unboxed as VV

data Land
 = Land
 { _lSeed :: !Double
 , _lMag  :: !Double
 , _lRate :: !Double
 , _lRes  :: !Int
 }

landDefault :: Land
landDefault
 = Land 1532 32 0.12 5

grab :: Land -> Double -> Double -> VV.Vector (Double, Double)
grab land x around -- from to step
 = let by    = _lRes land
       from  = fromIntegral  (truncate ((x - around) / fromIntegral by) * by)
       to    = from + around*2
       num   = truncate ((to - from) / fromIntegral by)
       gen i = let x' = from + fromIntegral (i * by)
               in  (x', get1 land x')
   in  VV.generate num gen

get1 :: Land -> Double -> Double
get1 !land x
 = let seed  = _lSeed land
       mag   = _lMag  land
       rate  = _lRate land
   in  sin (sin (seed + sin (x * rate) * rate) * mag
     + sin (seed + sin (x * rate * 0.2) * rate * 2) * (mag / 4)) * (mag / 2)
     + sin (x * rate * 0.15) * (mag / 3)
     + sin (x * rate * 16 * sin x) * (mag/32) * sin (x * 0.01 + sin (x * 0.01))
     - 40

getYNorm :: Land -> Double -> (Double, V.Normal2)
getYNorm land x
 = let by    = _lRes land
       by'   = fromIntegral by
       x'    = fromIntegral  (truncate (x / by') * by)
       y1    = get1 land  x'
       y2    = get1 land (x' + by')
       diff  = (x - x') / by'
       y'    = y1 * (1 - diff) + y2 * diff
   in (y', V.mkNormal $ V.Vec2 (y1 - y2) by')

