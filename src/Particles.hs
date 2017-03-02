{-# LANGUAGE RecordWildCards #-}

module Particles where

import Prelude hiding ((.)) -- To use (.) in the scope of Categories instead
import Control.Wire 
import FRP.Netwire 
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.FTGL as FTGL
import System.Random
import Control.Monad hiding (unless,when)
import Numeric 
import Lib
import FRP.Netwire.Input
import FRP.Netwire.Input.GLFW
import Debug.Trace
import Data.Either (rights,isRight)

data Particle = Particle
    { particleX :: Float
    , particleY :: Float
    , particleV :: Float
    } deriving Show

swirlUp :: (HasTime t s, Monad m) => (Float,Float) -> Wire s () m a (Point,Point,Float,Float)
swirlUp (x,y) = (,,,) <$> pos <*> pure (0,0) <*> r <*> pure 0.5
    where pos = (,) <$> ((/4) . sin <$> integral x . pure (pi/2)) <*> integral y . pure 0.4
          r = -pi/2

circling :: (HasTime t s, Monad m) => Float -> Wire s () m a (Point,Point,Float,Float)
circling raccel = (\r' pos'@(x,y) v -> (rotatePoint (0,0) r' pos',(0,0),r',v)) <$> r <*> pos <*> pure 0.5
    where r = integral 0 . pure raccel
          pos = (,) <$> 0 <*> (-0.8)


--testExpel = testParticle $ expel 4 200 (randomRs (2*pi-0.3,2*pi+0.3) $ mkStdGen 11) thrustParticleSpeed [] . circling (-1)
testFlyCircle = testParticle $ thruster . circling (-1)
testFlyUp = testParticle $ thruster . swirlUp (0,-1)

explosion :: (HasTime t s, Fractional t, Monad m) => Float -> Float -> Wire s () m a Float
explosion speed accel = when (if accel > 0 then (<0) else (>0)) . integral speed . pure accel --> for 0.3 . pure 1000 --> pure 0
                                                                 -- FIXME replace workaround with inhibit
                                                                 -- and findout how to remove inhibted
                                                                 -- wires from a list

--braking :: (HasTime t s, Fractional t, Monad m) => Point -> Point -> Wire s () m a Point
                                                
thrustParticleSpeed :: (HasTime t s, Fractional t, Monad m) => Float -> Wire s () m a Float
thrustParticleSpeed v = pure v--explosion v (-v*2)

randDelayWiresWith :: (Fractional t, HasTime t s, Monoid e, Monad m) => (Float,Float) -> Wire s e m a b -> [Wire s e m a b] -> [Wire s e m a b]
randDelayWiresWith (f,t) placeholder wires = zipWith 
    (\w t -> for t . placeholder --> w) 
    wires 
    $ map (fromRational . toRational) $ randomRs (f,t) $ mkStdGen 3

--particles :: (HasTime t s, Monad m) => (Float, Float) -> Float -> Float -> Wire s () m a [Point]
--particles origin speed accel = sequenceA $ map (particle origin $ explosion speed accel) [0..360]

thruster :: (HasTime t s, Monad m, Fractional t) => Wire s () m (Point,Point,Float,Float) [Particle]
thruster = expel 3 0.3 (mkStdGen 12) thrustParticleSpeed []

expel :: (HasTime t s, Monad m, Fractional t, RandomGen g) 
                => Int 
                -> Float
                -> g 
                -> (Float -> Wire s () m a Float) 
                -> [Wire s () m a Particle]
                -> Wire s () m (Point,Point,Float,Float) [Particle]
expel newN angleMax seeder speedWire particleWires = mkGen $ \ds (origin,speed,r,a) -> do
    let (angleSeed,g') = random seeder
        angles = take newN $ randomRs (-angleMax,angleMax) $ mkStdGen angleSeed 
        newParticle angle = for (fromRational $ toRational $ 1 - abs angle)  . particle origin speed (speedWire a) (angle+r)
        newParticles = map newParticle angles
        updatedParticleWires = if a /= 0 then newParticles ++ particleWires else particleWires
    (particles,particleWires') <- fmap (unzip.(filter (\(p,_) -> isRight p))) $ sequenceA $ map (\w -> stepWire w ds $ Right undefined) updatedParticleWires
    return (sequenceA particles, expel newN angleMax g' speedWire particleWires')

{--
thruster origin speed r = 
    particleCone origin 
        (randDelayWiresWith (0,10) (pure 0) $ map (\v -> explosion v (-v)) ds)
    r (0.3)
    where n = 567 -- truncate $ 1000 * speed
          ds = take n $ randomRs (speed-speed/10,speed+speed/10) $ mkStdGen 9
--}
particleCone :: (HasTime t s, Monad m) => (Float, Float) -> [Wire s () m a Float] -> Float -> Float -> Wire s () m a [Particle]
particleCone origin speedWires offsetR r = sequenceA $ zipWith (particle origin (0,0)) speedWires $ randomRs range (mkStdGen 1)
    where range = (offsetR-r/2,offsetR+r/2)

particle :: (HasTime t s, Monad m) => (Float, Float) -> (Float,Float) -> Wire s () m a Float -> Float -> Wire s () m a Particle
particle (x,y) (vX,vY) speedWire r = Particle <$> posX <*> posY <*> speedWire
    where vs   = (\(vX',vY') -> (vX+vX',vY+vY')) . (\d -> rotatePoint (0,0) r (d, 0)) <$> speedWire
          posX = integral x . (fst <$> vs)
          posY = integral y . (snd <$> vs)

recycle :: Monad m => Wire s e m a b -> Wire s e m a b
recycle p = p --> recycle p

renderParticles :: [Particle] -> IO ()
renderParticles = GL.renderPrimitive GL.Points . mapM_ renderPoint . map (\Particle {..} -> (particleX, particleY))

renderThrustParticles :: [Particle] -> IO ()
renderThrustParticles = GL.renderPrimitive GL.Points . mapM_ (\Particle {..} -> do
    GL.color $ GL.Color4 1 0.5 (0 :: GL.GLfloat) 0.5
    renderPoint (particleX,particleY)
    --renderPoint (particleX+size,particleY)
    --renderPoint (particleX+size,particleY+size)
    --renderPoint (particleX,particleY+size)
    )
    where size = 0.007

runParticle :: GLFW.Window -> Wire (Timed NominalDiffTime ()) () IO a [Particle] -> IO ()
runParticle window particleWire = do
    g <- getStdGen
    runNetwork clockSession_ particleWire
    where runNetwork sess wire = do
            GLFW.pollEvents
            (st,sess') <- stepSession sess
            (particles,wire') <- stepWire wire st $ Right undefined
            shouldClose <- GLFW.windowShouldClose window
            if shouldClose
            then return ()
            else case particles of
                Left _ -> runNetwork sess' wire'
                Right particles -> do
                    GL.clearColor GL.$= GL.Color4 0.0 0.0 0.0 1
                    GL.clear [GL.ColorBuffer]
                    renderThrustParticles particles 
                    GL.flush
                    GLFW.swapBuffers window
                    runNetwork sess' wire'

testExplosion v = testParticle $ boom (0,0) v

boom (x,y) v = mconcat $ map cone
    [randDelayWiresWith (0.2,0.6) 0 $ map (\v -> explosion v (-v*3)) $ vs 100
    ,randDelayWiresWith (0.0,0.3) 0 $ map (\v -> explosion v (-v*1)) $ vs 150
    ,randDelayWiresWith (0.0,0.4) 0 $ map (\v -> explosion (v*1.5) (-v)) $ vs 300
    ]
    where --delays = randDelayWiresWith (0.2,3) 0
          cone ws = particleCone (x,y) ws 0 (pi*2)
          vs n = take n $ randomRs (v-v/2,v+v/2) $ mkStdGen 10

testParticle :: Wire (Timed NominalDiffTime ()) () IO a [Particle] -> IO ()
testParticle wire = do
    GLFW.init
    (Just window) <- GLFW.createWindow 800 800 "Particle Demo" Nothing Nothing
    GLFW.makeContextCurrent (Just window)
    runParticle window wire
    GLFW.destroyWindow window
    GLFW.terminate
