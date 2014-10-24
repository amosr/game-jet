module State where

import qualified Data.Vect.Double                   as V
-- import qualified Data.Vect.Double.Util.Quaternion   as Q

import qualified Graphics.UI.GLFW                   as GLFW

import qualified Plane                              as P
import qualified Land                               as L
import Data.IORef


data State
 = State
 { _sFrames     :: IORef Int
 , _sTime       :: IORef Double
 , _sFrameTime  :: IORef Double
 , _sCursorPos  :: IORef (Double, Double)
 , _sPlane      :: IORef P.Plane
 , _sPlaneControl :: IORef P.PlaneControl
 , _sLand       :: IORef L.Land
 }

initialise :: GLFW.Window -> IO State
initialise win
 = do   frames' <- newIORef   0
        time'   <- newIORef   0
        ft'     <- newIORef   0

        pos     <- GLFW.getCursorPos win
        pos'    <- newIORef   pos

        plane'  <- newIORef P.plane
        pc'     <- newIORef P.planeControl

        land'   <- newIORef L.landDefault

        return State
               { _sFrames    = frames'
               , _sTime      = time'
               , _sFrameTime = ft'
               , _sCursorPos = pos'
               , _sPlane     = plane'
               , _sPlaneControl = pc' 
               , _sLand         = land'
               }


update :: GLFW.Window -> State -> IO ()
update win s
 = do   modifyIORef (_sFrames s) (+1)

        oldtime<- readIORef $ _sTime s
        time'  <- GLFW.getTime
        case time' of
         Just t  -> do
            writeIORef (_sTime s) t
            writeIORef (_sFrameTime s) (t - oldtime)
         Nothing -> return ()

        frametime <- readIORef $ _sFrameTime s

        plane <- readIORef $ _sPlane        s
        pc    <- readIORef $ _sPlaneControl s

        let plane' = P.fly pc plane frametime

        -- TODO collide properly
        land <- readIORef $ _sLand s
        let po = P._pOrg plane'
            (y,norm) = L.getYNorm land (V._1 po)
            uppy v
             -- Check if it's going towards the normal
             | v V.&. (V.fromNormal norm) < 0 -- V._2 v < 0 -- v V.&. (V.fromNormal norm) < 0
             = let v' = dim v
               in  (v' V.&+ diffo)  V.&- (V.project' v' norm V.&* 1.1)
             -- v V.&* (-1) -- V.project' v norm -- v V.&* (-1)
             | otherwise
             = dim v V.&+ diffo
            dotty
             = V.Vec2 0 (V._2 po - y) V.&. V.fromNormal norm
            diffo
             | dotty < 0
             = V.fromNormal norm V.&* (- dotty)
             | otherwise
             = V.Vec2 0 0
            dim v = v V.&* 0.9

            plane''
             | V._2 po < y
             = plane' { P._pVel = uppy (P._pVel plane') } -- , P._pOrg = V.Vec2 (V._1 po) (y + 5) }
             | otherwise
             = plane'

        putStrLn (show dotty)

        writeIORef (_sPlane s) plane''

        -- Get mouse cursor position
        _oldpos <- readIORef $ _sCursorPos s
        newpos <- GLFW.getCursorPos win

        writeIORef (_sCursorPos s) newpos

