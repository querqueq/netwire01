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
import qualified Data.MemoCombinators as Memo


title :: String
title = "Netwire 01"

f :: FT
f = 1.5

s :: FT
s = 0.02 / f

mainAcceleration :: FT
mainAcceleration = 0.5 / f

maneuveringAcceleration :: FT
maneuveringAcceleration = mainAcceleration / 2

rotationalAcceleration :: FT
rotationalAcceleration = 2

data Frame = Frame
    { playerShip    :: Ship
    , playerThrust  :: [Particle]
    , cam           :: Point
    } deriving Show

frameWire :: (HasTime t s, Fractional t) => InputWire s () Frame
frameWire = Frame 
        <$> shipWire 
        <*> thrustsWire
        <*> ((\(Ship {..}) -> (posX,posY)) <$> shipWire)

data Ship = Ship
    { posX      :: FT
    , posY      :: FT
    , aX        :: FT
    , aY        :: FT
    , vX        :: FT
    , vY        :: FT
    , aR        :: FT
    , vR        :: FT
    , dR        :: FT
    , thrusters :: Thrusters
    } deriving Show

thrusterOrigin :: (HasTime t s) => (Ship -> Thruster) -> InputWire s Ship (Point,Point,FT,FT)
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
    { thrusterOffsetX   :: FT
    , thrusterOffsetY   :: FT
    , thrusterOffsetR   :: FT
    , thrusterThrust    :: FT
    } deriving Show

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

generatePoints :: FT -> FT -> FT -> [(FT, FT)]
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

stars :: (RandomGen g) => g -> Point -> FT -> [Star2]
stars g (xOffset,yOffset) range = zipWith (\(x,y) z -> (x+xOffset,y+yOffset,z))
    (pairs $ randomRs (-range,range) g)
    (randomRs (starNear,starFar) g)

type Star2 = (FT,FT,FT)

starsAt :: Point -> [Star2]
starsAt (x,y) = foldl (\ss (xo,yo) -> starsAt' (xo+x,yo+y) ++ ss) [] 
    [(-r,r ),(0 ,r ),(r ,r )
    ,(-r,0 ),(0 ,0 ),(r ,0 )
    ,(-r,-r),(0 ,-r),(r ,-r)
    ]
    where r = 4

starsAt' :: Point -> [Star2]
starsAt' (x,y) = hundredStarsAt (f x,f y) where f = fromIntegral . fst . withinRange 4

hundredStarsAt :: (Int,Int) -> [Star2]
hundredStarsAt = (Memo.pair Memo.integral Memo.integral) f
    where f (x,y) = take 111 $ stars (mkStdGen $ truncate $ fromIntegral $ x+y) (fromIntegral x,fromIntegral y) 4

withinRange :: Integer -> FT -> (Integer,Integer)
withinRange j x = (x'-a,x'-a+j)
    where a = x' `mod` j 
          x' = truncate x

starNear :: FT
starNear = -200
starFar :: FT
starFar = -500

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
            GL.renderPrimitive GL.Quads 
                $ mapM_ renderPoint 
                $ map (rotatePoint (posX,posY) dR)
                $ generatePoints posX posY s
            --GL.preservingMatrix $ do
            --GL.matrixMode $= GL.Modelview 0
            GL.loadIdentity
            GL.perspective 1 1 1 3000
            GL.lookAt (GL.Vertex3 camX camY 1) (GL.Vertex3 camX camY 0) (GL.Vector3 0 1 0)
            GL.flush
            GLFW.swapBuffers window
            runNetwork font window inptCtrl inpt'' session' wire'

xaccel' :: InputWire s () FT
xaccel' = (+) <$> leftThrust <*> rightThrust

xspeed' :: HasTime t s => InputWire s () FT
xspeed' = integral 0 . (fst <$> rxyaccel)

yaccel' :: InputWire s () FT
yaccel' = (+) <$> frontThrust <*> backThrust

yspeed' :: HasTime t s => InputWire s () FT
yspeed' = integral 0 . (snd <$> rxyaccel)

raccel' :: InputWire s () FT
raccel' = (+) <$> rotateLeftThrust <*> rotateRightThrust

rspeed' :: HasTime t s => InputWire s () FT
rspeed' = integral 0 . raccel'

rdegree' :: HasTime t s => InputWire s () FT
rdegree' = (`mod'` (2*pi)) <$> integral 0 . raccel'

--negateInertia :: Monoid e => Wire s e IO () FT

pos :: HasTime t s => InputWire s () (FT, FT)
pos = (,) <$> integral 0 . (fst <$> rspeeds') <*> integral 0 . (snd <$> rspeeds')

rxyaccel :: HasTime t s => InputWire s () (FT, FT)
rxyaccel = (\d (aX,aY) -> rotatePoint (0,0) d (aX,aY)) <$> rdegree' <*> ((,) <$> xaccel' <*> yaccel')

rspeeds' :: HasTime t s => InputWire s () (FT,FT)
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
