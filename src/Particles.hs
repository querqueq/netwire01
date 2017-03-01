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
import Data.Either (rights)

data Particle = Particle
    { particleX :: Float
    , particleY :: Float
    , particleV :: Float
    } deriving Show

swirlUp :: (HasTime t s, Monad m) => (Float,Float) -> Wire s () m a (Point,Float,Float)
swirlUp (x,y) = (,,) <$> pos <*> r <*> pure 0.5
    where pos = (,) <$> ((/4) . sin <$> integral x . pure (pi/2)) <*> integral y . pure 0.2
          r = 0

circling :: (HasTime t s, Monad m) => Float -> Wire s () m a (Point,Float,Float)
circling raccel = (\r' pos'@(x,y) v -> (rotatePoint (0,0) r' pos',r',v)) <$> r <*> pos <*> pure 0.5
    where r = integral 0 . pure raccel
          pos = (,) <$> 0 <*> (-0.8)


testExpel = testParticle $ expel 4 200 (randomRs (2*pi-0.3,2*pi+0.3) $ mkStdGen 11) thrustParticleSpeed (pure []) . circling (-1)
testFlyCircle = testParticle $ thruster . circling (-1)

explosion :: (HasTime t s, Fractional t, Monad m) => Float -> Float -> Wire s () m a Float
explosion speed accel = when (if accel > 0 then (<0) else (>0)) . integral speed . pure accel --> for 0.3 . pure 1000 --> pure 0
                                                                 -- FIXME replace workaround with inhibit
                                                                 -- and findout how to remove inhibted
                                                                 -- wires from a list
                                                
thrustParticleSpeed :: (HasTime t s, Fractional t, Monad m) => Float -> Wire s () m a Float
thrustParticleSpeed v = explosion v (-v*2)

randDelayWiresWith :: (Fractional t, HasTime t s, Monoid e, Monad m) => (Float,Float) -> Wire s e m a b -> [Wire s e m a b] -> [Wire s e m a b]
randDelayWiresWith (f,t) placeholder wires = zipWith 
    (\w t -> for t . placeholder --> w) 
    wires 
    $ map (fromRational . toRational) $ randomRs (f,t) $ mkStdGen 3

--particles :: (HasTime t s, Monad m) => (Float, Float) -> Float -> Float -> Wire s () m a [Point]
--particles origin speed accel = sequenceA $ map (particle origin $ explosion speed accel) [0..360]

thruster :: (HasTime t s, Monad m, Fractional t) => Wire s () m (Point,Float,Float) [Particle]
thruster = expel 3 200 range thrustParticleSpeed (pure [])
    where range = randomRs (-0.4,0.4) $ mkStdGen 12

expel :: (HasTime t s, Monad m) => Int -> Int -> [Float] -> (Float -> Wire s () m a Float) -> Wire s () m a [Particle] -> Wire s () m (Point,Float,Float) [Particle]
expel newN maxN rands speedWire particlesWire = mkGen $ \ds (origin,r,a) -> do
    let angles = take newN rands
        newParticle angle = particle origin (speedWire a) (angle+r)
        newParticles = sequenceA $ map newParticle angles
        updatedParticlesWire = if a /= 0 then fmap (take maxN) $ (++) <$> newParticles <*> particlesWire else particlesWire
    -- TODO remove inhibited particles
    (particles,particlesWire') <- stepWire updatedParticlesWire ds $ Right undefined 
    return (particles, expel newN maxN (drop newN rands) speedWire particlesWire')

{--
thruster origin speed r = 
    particleCone origin 
        (randDelayWiresWith (0,10) (pure 0) $ map (\v -> explosion v (-v)) ds)
    r (0.3)
    where n = 567 -- truncate $ 1000 * speed
          ds = take n $ randomRs (speed-speed/10,speed+speed/10) $ mkStdGen 9
--}
particleCone :: (HasTime t s, Monad m) => (Float, Float) -> [Wire s () m a Float] -> Float -> Float -> Wire s () m a [Particle]
particleCone origin speedWires offsetR r = sequenceA $ zipWith (particle origin) speedWires $ randomRs range (mkStdGen 1)
    where range = (offsetR-r/2,offsetR+r/2)

particle :: (HasTime t s, Monad m) => (Float, Float) -> Wire s () m a Float -> Float -> Wire s () m a Particle
particle (x,y) speedWire r = Particle <$> (fst <$> pos) <*> (snd <$> pos) <*> speedWire
    where pos = (\(x',y') -> (x+x',y+y')) . (\d -> rotatePoint (0,0) r (d, 0)) <$> integral 0 . speedWire

recycle :: Monad m => Wire s e m a b -> Wire s e m a b
recycle p = p --> recycle p

renderParticles :: [Particle] -> IO ()
renderParticles = GL.renderPrimitive GL.Points . mapM_ renderPoint . map (\Particle {..} -> (particleX, particleY))

renderThrustParticles :: [Particle] -> IO ()
renderThrustParticles = GL.renderPrimitive GL.Points . mapM_ (\Particle {..} -> do
    GL.color $ GL.Color4 1 (0.7 - particleV) (0 :: GL.GLfloat) 0.5
    renderPoint (particleX,particleY)
   -- renderPoint (particleX+0.005,particleY)
   -- renderPoint (particleX+0.005,particleY+0.005)
   -- renderPoint (particleX,particleY+0.005)
    )

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
