{-# LANGUAGE RecordWildCards #-}

-- Ship physics and thruster exhaust, moved here from app/Main.hs and
-- parameterized on an abstract thrust input so that both frontends drive the
-- same dynamics: the desktop maps GLFW keys to a Thrust value, the web maps
-- its control bitmask. This module must build on both GHC 8.0.2 (stack) and
-- GHC 9.12 (wasm32-wasi).
module Core.Ship where

import Prelude hiding ((.)) -- To use (.) in the scope of Categories instead
import Control.Wire
import FRP.Netwire
import Control.Arrow (arr)
import Data.Fixed (mod')
import Core.Geometry
import Core.Particles

mainAcceleration :: FT
mainAcceleration = 0.5 / 1.5

maneuveringAcceleration :: FT
maneuveringAcceleration = mainAcceleration / 2

rotationalAcceleration :: FT
rotationalAcceleration = 2

-- Signed acceleration applied by each control this instant (0 = inactive).
-- Conventions match the original key wires in app/Main.hs: front is positive,
-- back negative, right strafe positive, left negative, rotate-left positive,
-- rotate-right negative.
data Thrust = Thrust
    { thrustFront       :: FT
    , thrustBack        :: FT
    , thrustLeft        :: FT
    , thrustRight       :: FT
    , thrustRotLeft     :: FT
    , thrustRotRight    :: FT
    } deriving Show

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

allThrusters :: Thrusters -> [Thruster]
allThrusters (Thrusters {..}) = [thrusterFront, thrusterBack, thrusterLeft, thrusterRight]

thrusterOrigin :: (Ship -> Thruster) -> Wire s e m Ship (Point,Point,FT,FT)
thrusterOrigin t = mkPure_ $ \ship@(Ship {..}) -> do
    Right $ ((posX,posY),(vX,vY),dR+(thrusterOffsetR $ t ship),thrusterThrust $ t ship)

-- The ship integrator, structured exactly like the original wires in
-- app/Main.hs (xaccel'/rdegree'/pos/...), but reading thrust levels from the
-- input instead of from GLFW key wires. Note the original quirk, preserved
-- here: the heading integrates the rotational *acceleration* once, so it
-- effectively acts as a rotational velocity.
shipWire :: (HasTime t s, Monad m) => Wire s () m Thrust Ship
shipWire = Ship
       <$> (fst <$> pos)
       <*> (snd <$> pos)
       <*> xaccel
       <*> yaccel
       <*> xspeed
       <*> yspeed
       <*> raccel
       <*> rspeed
       <*> rdegree
       <*> (Thrusters <$> (Thruster 0 0 (3*pi/2) <$> arr thrustFront)
                      <*> (Thruster 0 0 (3*pi/2) <$> arr thrustBack)
                      <*> (Thruster 0 0 pi       <$> arr thrustRight)
                      <*> (Thruster 0 0 pi       <$> arr thrustLeft)
           )
    where
      xaccel   = arr (\t -> thrustLeft t + thrustRight t)
      yaccel   = arr (\t -> thrustFront t + thrustBack t)
      raccel   = arr (\t -> thrustRotLeft t + thrustRotRight t)
      rspeed   = integral 0 . raccel
      rdegree  = (`mod'` (2*pi)) <$> integral 0 . raccel
      -- Thrust is applied relative to the ship's heading.
      rxyaccel = (\d (aX',aY') -> rotatePoint (0,0) d (aX',aY')) <$> rdegree <*> ((,) <$> xaccel <*> yaccel)
      xspeed   = integral 0 . (fst <$> rxyaccel)
      yspeed   = integral 0 . (snd <$> rxyaccel)
      pos      = (,) <$> integral 0 . xspeed <*> integral 0 . yspeed

-- Exhaust particles for all four thrusters of a ship, as in the original
-- thrustsWire (including its thruster order and the left/right pairing).
exhaustWire :: (HasTime t s, Fractional t, Monad m) => Wire s () m Ship [Particle]
exhaustWire =
      mconcat
    $ map (\t -> thruster . thrusterOrigin (t . thrusters))
      [thrusterRight, thrusterFront, thrusterBack, thrusterLeft]
