module RenderLand where

import qualified Data.Vect.Double                   as V
import qualified Data.Vect.Double.OpenGL            as VGL

import qualified Data.Vector.Unboxed                as VV

import qualified Graphics.Rendering.OpenGL          as GL

import qualified Land                               as L

{-# INLINE draw #-}
draw :: L.Land
     -> V.Vec2
     -> IO ()
draw land v
 = let vs = L.grab land (V._1 v) 200
   in GL.renderPrimitive GL.TriangleStrip $ VV.mapM_ draw1 vs
 where
  draw1 (x, y)
   = do  let s = sin (x * 0.01) -- (sin (cos (x * 0.1) * 0.1) * y) / 50
         color (0.3 +  (-0.2 * s), 0.5 + 0.3 * s, 0.2 + (-0.1*s))
         GL.vertex (V.Vec3 x y   (-200))
         color (0.1, 0.1, 0.1)
         GL.vertex (V.Vec3 x (-100) (-200))

  
  color (r,g,b)
   = GL.color $ GL.Color4 (VGL.glflt r) (VGL.glflt g) (VGL.glflt b) 0.1



