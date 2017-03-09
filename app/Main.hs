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
import Stars
import Lib
import Particles

title :: String
title = "Netwire 01"

f :: Float
f = 1.5

s :: Float
s = 0.02 / f

mainAcceleration :: Float
mainAcceleration = 0.4 / f

maneuveringAcceleration :: Float
maneuveringAcceleration = mainAcceleration / 2

rotationalAcceleration :: Float
rotationalAcceleration = 2

{--
data GameState = GameState
    { playerShip    :: Ship
    , stars         :: [(Float,Float)]
    } deriving Show
--}

data Ship = Ship
    { posX      :: Float
    , posY      :: Float
    , aX        :: Float
    , aY        :: Float
    , vX        :: Float
    , vY        :: Float
    , aR        :: Float
    , vR        :: Float
    , dR        :: Float
    , thrusters :: Thrusters
    } deriving Show

thrusterOrigin :: (HasTime t s) => (Ship -> Thruster) -> InputWire s Ship (Point,Point,Float,Float)
thrusterOrigin t = mkPure_ $ \ship@(Ship {..}) -> do 
    Right $ ((posX,posY),(vX,vY),dR+(thrusterOffsetR $ t ship),thrusterThrust $ t ship)

allThrusters (Thrusters {..}) = [thrusterFront, thrusterBack, thrusterLeft, thrusterRight]

data Thrusters = Thrusters
    { thrusterFront     :: Thruster
    , thrusterBack      :: Thruster
    , thrusterLeft      :: Thruster
    , thrusterRight     :: Thruster
    } deriving Show

data Thruster = Thruster
    { thrusterOffsetX   :: Float
    , thrusterOffsetY   :: Float
    , thrusterOffsetR   :: Float
    , thrusterThrust    :: Float
    } deriving Show

prettyShow :: Ship -> String
prettyShow (Ship {..}) = foldr (\(name,val,unit) xs -> name ++ ": " ++ format val ++ unit ++ " " ++ xs) "" 
    [("X", posX, "")
    ,("Y", posY, "")
    ,("Rotation", dR * 180 / pi, "°")
    ,("Y-Thrust", aY, "")
    ,("X-Thrust", aX, "")
    ,("R-Thrust", aR, "")
    ]

thrust :: [GLFW.Key] -> Float -> InputWire s () Float
thrust ks a = pure a . (foldr (<|>) (keyPressed GLFW.Key'Y) $ map keyPressed ks) <|> pure 0

frontThrust         = thrust [GLFW.Key'W,GLFW.Key'Up,GLFW.Key'X] mainAcceleration
leftThrust          = thrust [GLFW.Key'A,GLFW.Key'Left,GLFW.Key'X] (-maneuveringAcceleration)
backThrust          = thrust [GLFW.Key'S,GLFW.Key'Down,GLFW.Key'X] (-maneuveringAcceleration)
rightThrust         = thrust [GLFW.Key'D,GLFW.Key'Right,GLFW.Key'X] maneuveringAcceleration
rotateLeftThrust    = thrust [GLFW.Key'Q] rotationalAcceleration
rotateRightThrust   = thrust [GLFW.Key'E] (-rotationalAcceleration)


shipWire :: (HasTime t s) => InputWire s () Ship
shipWire = Ship
       <$> (fst <$> pos)
       <*> (snd <$> pos)
       <*> xaccel'
       <*> yaccel'
       <*> xspeed'
       <*> yspeed'
       <*> raccel'
       <*> rspeed'
       <*> rdegree'
       <*> (Thrusters <$> (Thruster <$> pure 0 <*> pure 0 <*> pure (3*pi/2) <*> frontThrust)
                      <*> (Thruster <$> pure 0 <*> pure 0 <*> pure (3*pi/2) <*> backThrust)
                      <*> (Thruster <$> pure 0 <*> pure 0 <*> pure pi <*> rightThrust)
                      <*> (Thruster <$> pure 0 <*> pure 0 <*> pure pi <*> leftThrust)
           )

generatePoints :: Float -> Float -> Float -> [(Float, Float)]
generatePoints x y s =
    [ (x - s, y - s)
    , (x + s, y - s)
    , (x + s2, y + s)
    , (x - s2, y + s)
    ] where s2 = s / 4

thrustsWire :: (Fractional t, HasTime t s) => InputWire s () [Particle]
thrustsWire = 
      mconcat 
    $ map (thruster.) 
    $ zipWith (\t s -> (thrusterOrigin $ t . thrusters) . s) 
      [thrusterRight, thrusterFront, thrusterBack, thrusterLeft]
    $ repeat shipWire

fromToRational = fromRational . toRational

pairs :: [a] -> [(a,a)]
pairs [] = []
pairs (a:(b:cs)) = (a,b) : pairs cs

stars :: (RandomGen g) => g -> Float -> [(Float,Float,Float)]
stars g range = zipWith (\(x,y) z -> (x,y,z))
    (pairs $ randomRs (-range,range) g)
    (randomRs (starNear,starFar/2) g)

starNear = -200
starFar = -1000

starColor :: Float -> GL.Color3 GL.GLfloat
starColor depth = GL.Color3 v v v
    where depth' = abs depth
          v = (+) 0.1  $ (-) 1 $ normalize (abs starNear,abs starFar) depth'
          
renderStar2 :: (Float,Float,Float) -> IO ()
renderStar2 (x,y,z) = GL.renderPrimitive GL.Points $ do 
    GL.color $ starColor z
    GL.vertex $ GL.Vertex3 x y z

normalize (min,max) val = (val - min) / (max - min)

run :: FTGL.Font -> GLFW.Window -> GLFWInputControl -> IO ()
run font window inptCtrl = do
        inpt <- getInput inptCtrl
--        xys <- zip <$> f <*> f
--        let ss = map (\(x,y) -> Star x y 0 1) xys
        g <- getStdGen
        let ss = take 3000 $ stars g 10
        runNetwork font window inptCtrl inpt clockSession_ 
            shipWire 
            --(skyWire g 678) 
            ss
            thrustsWire
            --(thruster . frontThrusterOrigin . shipWire)


runNetwork :: (HasTime t s, Fractional t)
                         => FTGL.Font
                         -> GLFW.Window
                         -> GLFWInputControl
                         -> GLFWInputState
                         -> Session IO s
                         -> InputWire s () Ship
                         -- -> Wire s e IO a [Star]
                         -> [(Float,Float,Float)]
                         -> InputWire s () [Particle]
                         -> IO ()
runNetwork font window inptCtrl inpt session wire ss ft = do
    --GLFW.pollEvents
    inpt' <- pollGLFW inpt inptCtrl
    (st , session') <- stepSession session
    ((ftParticles,ft'),_) <- runGLFWInputT (stepWire ft st $ Right undefined) inpt'
    ((wt', wire'), inpt'') <-
        runGLFWInputT (stepWire wire st $ Right undefined) inpt'
    --(stars, sky') <- stepWire sky st $ Right undefined
    shouldClose <- GLFW.windowShouldClose window
    if shouldClose
    then return ()
    else case wt' of
        Left _ -> return ()
        Right ship@(Ship {..}) -> do
            --GL.loadIdentity
            GL.clearColor GL.$= GL.Color4 0.0 0.0 0.0 1
            GL.clear [GL.ColorBuffer, GL.DepthBuffer]
            let d = (-100.333 :: GL.GLfloat)
            mapM_ renderStar2 ss
            case ftParticles of 
                Left _ -> return ()
                Right particles -> renderThrustParticles particles
            FTGL.renderFont font (prettyShow ship) FTGL.Front
            {--
            case stars of 
                Left _ -> return ()
                Right stars -> mapM_ renderStar stars
                --}
            GL.color $ GL.Color3 1 1 (1 :: GL.GLfloat)
            GL.renderPrimitive GL.Quads 
                $ mapM_ renderPoint 
                $ map (rotatePoint (posX,posY) dR)
                $ generatePoints posX posY s
            --GL.preservingMatrix $ do
            --GL.matrixMode $= GL.Modelview 0
            GL.loadIdentity
            GL.perspective 1 1 1 3000
            GL.lookAt (GL.Vertex3 (fromToRational posX) (fromToRational posY) 1) (GL.Vertex3 (fromToRational posX) (fromToRational posY) 0) (GL.Vector3 0 1 0)
            GL.flush
            GLFW.swapBuffers window
            runNetwork font window inptCtrl inpt'' session' wire' ss ft'

xaccel' :: InputWire s () Float
xaccel' = (+) <$> leftThrust <*> rightThrust

xspeed' :: HasTime t s => InputWire s () Float
xspeed' = integral 0 . (fst <$> rxyaccel)

yaccel' :: InputWire s () Float
yaccel' = (+) <$> frontThrust <*> backThrust

yspeed' :: HasTime t s => InputWire s () Float
yspeed' = integral 0 . (snd <$> rxyaccel)

raccel' :: InputWire s () Float
raccel' = (+) <$> rotateLeftThrust <*> rotateRightThrust

rspeed' :: HasTime t s => InputWire s () Float
rspeed' = integral 0 . raccel'

rdegree' :: HasTime t s => InputWire s () Float
rdegree' = (`mod'` (2*pi)) <$> integral 0 . raccel'

--negateInertia :: Monoid e => Wire s e IO () Float

pos :: HasTime t s => InputWire s () (Float, Float)
pos = (,) <$> integral 0 . (fst <$> rspeeds') <*> integral 0 . (snd <$> rspeeds')

rxyaccel :: HasTime t s => InputWire s () (Float, Float)
rxyaccel = (\d (aX,aY) -> rotatePoint (0,0) d (aX,aY)) <$> rdegree' <*> ((,) <$> xaccel' <*> yaccel')

rspeeds' :: HasTime t s => InputWire s () (Float,Float)
rspeeds' = (,) <$> xspeed' <*> yspeed'


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
