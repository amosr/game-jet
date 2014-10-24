module RenderSky where
-- Render a skybox

import qualified Data.Vect.Double                   as V
import qualified Data.Vect.Double.OpenGL            as VGL

import qualified Graphics.Rendering.OpenGL          as GL

{-# INLINE draw #-}
draw :: Double
     -> V.Vec2
     -> IO ()
draw _time v
 = do   stars
        clouds
        mountains
 where
  stars
   = GL.preservingMatrix $ do
        VGL.glTranslate (V.extendWith (-500) (v V.&* (-0.01)))
        -- Big boxes for the stars
        GL.pointSize GL.$= 20
        -- Draw a few hundred points
        GL.renderPrimitive GL.Points $ mapM_ starN [0..300 :: Int]

  -- Position the nth star somewhere
  starN i
   = let i' = fromIntegral i
         x  = sin (1234901234 + sin (i' / 13) + i') * 500
         y  = sin (431224     + sin (i' / 27) + i') * 200
         dist
            = sin (12342      + sin (i' / 1234) + i')
         z  = dist * 50
         
     in  do
            color (0.5 + dist / 4, 0.5 + dist / 4, 0.5 + dist / 4)
            GL.vertex $ V.mkVec3 (x, y, z)


  clouds
   = GL.preservingMatrix $ do
        VGL.glTranslate (V.extendWith (-300) (v V.&* (-0.05)))
        -- Really big boxes for the clouds
        GL.pointSize GL.$= 200
        -- Draw a few hundred points
        GL.renderPrimitive GL.Points $ mapM_ cloudN [0..10 :: Int]

  cloudN i
   = let i' = fromIntegral i
         x  = sin (1234901234 + sin (i' / 13) + i') * 1000
         y  = sin (431224     + sin (i' / 27) + i') * 30 + 60
         dist
            = sin (12342      + sin (i' / 1234) + i')
         z  = dist * 50
         
     in  do
            color (0.0, 0.0, 0.0)
            GL.vertex $ V.mkVec3 (x - 6, y - 6, z - 2)
            GL.vertex $ V.mkVec3 (x + 10 - 6, y - 6, z - 2)

            color (0.4, 0.4, 0.4)
            GL.vertex $ V.mkVec3 (x - 3, y - 3, z - 1)
            GL.vertex $ V.mkVec3 (x + 10 - 3, y - 3, z - 1)

            color (0.8, 0.8, 0.8)
            GL.vertex $ V.mkVec3 (x, y, z)
            GL.vertex $ V.mkVec3 (x + 10, y, z)

  mountains
   = GL.preservingMatrix $ do
        VGL.glTranslate (V.extendWith (-300) (v V.&* (-0.03)))
        -- Really big boxes for the clouds
        GL.pointSize GL.$= 200
        -- Draw a few hundred points
        GL.renderPrimitive GL.Points $ mapM_ mountainN [0..1000 :: Int]

  mountainN i
   = let i' = fromIntegral i
         x  = sin (1234901234 + sin (i' / 13) + i') * 1000
         y  = sin (431224     + sin (i' / 27) + i') * 20 - 80
         dist
            = sin (12342      + sin (i' / 1234) + i')
         z  = dist * 150
         
     in  do
            color (0.1, 0.3 + dist / 4, 0.1)
            GL.vertex $ V.mkVec3 (x, y, z)
            color (0.3 + dist / 4, 0.3 + dist / 4, 0.1)
            GL.vertex $ V.mkVec3 (x, y - 10, z - 1)
            GL.vertex $ V.mkVec3 (x + 10, y - 10, z - 1)
            GL.vertex $ V.mkVec3 (x - 10, y - 10, z - 1)



  _trans mul
   = VGL.glTranslate (V.extendZero v V.&* negate mul)
  
  color (r,g,b)
   = GL.color $ GL.Color4 (VGL.glflt r) (VGL.glflt g) (VGL.glflt b) 0.1


