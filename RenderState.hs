module RenderState where
-- Glue the other renderers together

-- import qualified Data.Vect.Double.Util.Quaternion   as Q
-- import qualified Data.Vect.Double.OpenGL            as VGL
import qualified Data.Vect.Double                   as V
-- import qualified Data.Vect.Double.Util.Quaternion   as Q
import qualified Data.Vect.Double.OpenGL            as VGL


import qualified Graphics.Rendering.OpenGL          as GL
-- import qualified Graphics.Rendering.OpenGL.Raw      as GLR

import qualified State                              as S
import qualified RenderSky                          as RSky
import qualified RenderLand                         as RLand
import qualified Plane                              as P

import           Data.IORef

{-# INLINE draw #-}
draw :: S.State
     -> IO ()
draw s
 = do   -- org <- readIORef $ S._sOrg s
        -- rot <- readIORef $ S._sRot s
        time<- readIORef $ S._sTime s

        -- VGL.multMatrix  $ Q.leftOrthoU rot

        -- tun    <- readIORef $ S._sTunnel s
        -- Draw background colour
        -- let (r,g,b) = T.bgColour tun
        -- let col     = GL.Color4 (realToFrac r) (realToFrac g) (realToFrac b) 0.5

        -- Turn the fog back on only for the tunnel
        -- GL.fogColor   GL.$= col
        --GLR.glEnable GLR.gl_FOG

        plane <- readIORef $ S._sPlane s
        let po = P._pOrg plane
            pr = P._pRot plane
            extend v z = V.Vec3 (V._1 v) (V._2 v) z
            color (r,g,b)
                = GL.color $ GL.Color3 (VGL.glflt r) (VGL.glflt g) (VGL.glflt b)

        GL.clearColor GL.$= GL.Color4 (0.1) (0.1) (0.2) (1)
        GL.clear [GL.ColorBuffer]
        -- Draw sky box
        RSky.draw time po
    
        -- Clear depth buffer, as sky box should be behind everything
        GL.clear [GL.DepthBuffer]

        VGL.glTranslate $ V.Vec3 (negate $ V._1 po) (negate $ V._2 po / 20) 0

        land <- readIORef $ S._sLand s
        RLand.draw land po


        VGL.glTranslate $ extend po (-200)

        GL.renderPrimitive GL.Triangles $ do
            color (1.0, 0.2, 0.5)
            GL.vertex $ extend (V.sinCosRadius pr 5) 0
            GL.vertex $ extend (V.sinCosRadius (pr + 2.5) 5) 0
            GL.vertex $ extend (V.sinCosRadius (pr - 2.5) 5) 0

        -- VGL.glTranslate   org

        --GLR.glDisable GLR.gl_FOG


