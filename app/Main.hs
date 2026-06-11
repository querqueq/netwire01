{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Prelude hiding ((.)) -- To use (.) in the scope of Categories instead
import qualified Prelude as P ((.))
import Control.Wire
import qualified Graphics.Rendering.OpenGL.GLU.Matrix as GL
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import FRP.Netwire
import Data.IORef
import Data.Fixed (mod')
import Debug.Trace
import Control.Monad hiding (when,unless)
import Control.Monad.IO.Class
import Data.IORef
import Data.StateVar (($=))
import FRP.Netwire.Input
import FRP.Netwire.Input.GLFW
import qualified Graphics.Rendering.FTGL as FTGL
import Numeric
import System.Random
import Lib
import Particles
import Core.Ship
import Core.Stars


title :: String
title = "Netwire 01"

f :: FT
f = 1.5

s :: FT
s = 0.02 / f

data Frame = Frame
    { playerShip    :: Ship
    , playerThrust  :: [Particle]
    , cam           :: Point
    } deriving Show

-- The ship dynamics and exhaust particles come from netwire01-core
-- (Core.Ship), driven by the GLFW key wires below. As in the original
-- formulation, each use of shipW is an independent instance of the same
-- deterministic integrator, stepped with the same input and time delta.
frameWire :: (HasTime t s, Fractional t) => InputWire s () Frame
frameWire = (\ship particles -> Frame ship particles (posX ship, posY ship))
        <$> shipW
        <*> (exhaustWire . shipW)

shipW :: (HasTime t s) => InputWire s () Ship
shipW = shipWire . thrustInput

-- Current thrust levels from the GLFW key wires, fed to the shared ship wire.
thrustInput :: InputWire s () Thrust
thrustInput = Thrust
          <$> frontThrust
          <*> backThrust
          <*> leftThrust
          <*> rightThrust
          <*> rotateLeftThrust
          <*> rotateRightThrust

prettyShow :: Ship -> String
prettyShow (Ship {..}) = foldr (\(name,val,unit) xs -> name ++ ": " ++ format val ++ unit ++ " " ++ xs) ""
    [("X", posX, "")
    ,("Y", posY, "")
    ,("Rotation", dR * 180 / pi, "°")
    ,("X-Speed", vX, "")
    ,("Y-Speed", vY, "")
    --,("Y-Thrust", aY, "")
    --,("X-Thrust", aX, "")
    --,("R-Thrust", aR, "")
    ]

thrust :: [GLFW.Key] -> FT -> InputWire s () FT
thrust ks a = pure a . (foldr (<|>) (keyPressed GLFW.Key'Y) $ map keyPressed ks) <|> pure 0

frontThrust         = thrust [GLFW.Key'W,GLFW.Key'Up,GLFW.Key'X] mainAcceleration
leftThrust          = thrust [GLFW.Key'A,GLFW.Key'Left,GLFW.Key'X] (-maneuveringAcceleration)
backThrust          = thrust [GLFW.Key'S,GLFW.Key'Down,GLFW.Key'X] (-maneuveringAcceleration)
rightThrust         = thrust [GLFW.Key'D,GLFW.Key'Right,GLFW.Key'X] maneuveringAcceleration
rotateLeftThrust    = thrust [GLFW.Key'Q] rotationalAcceleration
rotateRightThrust   = thrust [GLFW.Key'E] (-rotationalAcceleration)

generatePoints :: FT -> FT -> FT -> [(FT, FT)]
generatePoints x y s =
    [ (x - s, y - s)
    , (x + s, y - s)
    , (x + s2, y + s)
    , (x - s2, y + s)
    ] where s2 = s / 4

fromToRational = fromRational . toRational

starColor :: FT -> GL.Color3 FT
starColor depth = GL.Color3 v v v
    where depth' = abs depth
          v = (+) 0.1  $ (-) 1 $ normalize (abs starNear,abs starFar) depth'

renderStar2 :: (FT,FT,FT) -> IO ()
renderStar2 (x,y,z) = GL.renderPrimitive GL.Points $ do
    GL.color $ starColor z
    GL.vertex $ GL.Vertex3 x y z

normalize (min,max) val = (val - min) / (max - min)

run :: FTGL.Font -> GLFW.Window -> GLFWInputControl -> IO ()
run font window inptCtrl = do
        inpt <- getInput inptCtrl
        g <- getStdGen
        runNetwork font window inptCtrl inpt clockSession_ frameWire

runNetwork :: (HasTime t s, Fractional t)
                         => FTGL.Font
                         -> GLFW.Window
                         -> GLFWInputControl
                         -> GLFWInputState
                         -> Session IO s
                         -> InputWire s () Frame
                         -> IO ()
runNetwork font window inptCtrl inpt session wire = do
    --GLFW.pollEvents
    inpt' <- pollGLFW inpt inptCtrl
    (st , session') <- stepSession session
    ((wt', wire'), inpt'') <- runGLFWInputT (stepWire wire st $ Right undefined) inpt'
    shouldClose <- GLFW.windowShouldClose window
    if shouldClose
    then return ()
    else case wt' of
        Left _ -> return ()
        Right (Frame { playerShip = ship@(Ship {..}), cam = cam, playerThrust = particles }) -> do
            let camX = fst cam
                camY = snd cam
            GL.clearColor GL.$= GL.Color4 0.0 0.0 0.0 1
            --GL.clear [GL.ColorBuffer, GL.DepthBuffer]
            GL.clear [GL.ColorBuffer]
            mapM_ renderStar2 $ starsAt (posX,posY)
            renderThrustParticles particles
            FTGL.renderFont font (prettyShow ship) FTGL.Front
            GL.color $ GL.Color3 1 1 (1 :: GL.GLfloat)
            {-- old ship
            GL.renderPrimitive GL.Quads
                $ mapM_ renderPoint
                $ map (rotatePoint (posX,posY) dR)
                $ generatePoints posX posY s
            --}
            GL.renderPrimitive GL.Polygon
                $ mapM_ renderPoint
                $ map (rotatePoint (posX,posY) dR)
                $ map (move (posX,posY))
                $ rocket 0.02
            --GL.preservingMatrix $ do
            --GL.matrixMode $= GL.Modelview 0
            GL.loadIdentity
            GL.perspective 1 1 1 3000
            GL.lookAt (GL.Vertex3 camX camY $ sqrt $ (*10000) $ (abs vX) + (abs vY) + 0.01) (GL.Vertex3 camX camY 0) (GL.Vector3 0 1 0)
            GL.flush
            GLFW.swapBuffers window
            runNetwork font window inptCtrl inpt'' session' wire'

main = do
    GLFW.init
    GL.depthFunc $= Just GL.Less
    (Just window) <- GLFW.createWindow 1080 1080 title Nothing Nothing
    GLFW.makeContextCurrent (Just window)
    font <- FTGL.createBitmapFont "DroidSansMono.ttf"
    FTGL.setFontFaceSize font 24 72
    mkInputControl window >>= run font window
    GLFW.destroyWindow window
    GLFW.terminate
